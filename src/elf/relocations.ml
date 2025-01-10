module IMap = Map.Make (Int)

type target =
  AArch64 of Abi_aarch64_symbolic_relocation.aarch64_relocation_target

type binary_operation = Elf_symbolic.binary_operation

type unary_operation = Elf_symbolic.unary_operation

type exp =
| Section of string
| Const of int
| BinOp of (exp * binary_operation * exp)
| UnOp of (unary_operation * exp)
(* | AssertRange of (exp * int * int) *)
| Mask of (exp * int * int)

type rel = {
  target : target;
  value : exp;
}

type t = rel IMap.t

type linksem_t = LinksemRelocatable.rels

let rec exp_of_linksem = function
| Elf_symbolic.Section s -> Section s
| Elf_symbolic.Const x -> Const (Z.to_int x)
| Elf_symbolic.BinOp (x, op, y) -> BinOp (exp_of_linksem x, op, exp_of_linksem y)
| Elf_symbolic.UnOp (op, x) -> UnOp (op, exp_of_linksem x)
| Elf_symbolic.AssertRange (x, _, _) -> exp_of_linksem x (* TODO *)
| Elf_symbolic.Mask (x, a, b) -> Mask (exp_of_linksem x, Z.to_int a, Z.to_int b)

let of_linksem: linksem_t -> t = function
| LinksemRelocatable.AArch64 relocs ->
    let add k Elf_symbolic.{ arel_value; arel_target } m =
      IMap.add (Z.to_int k) { value = exp_of_linksem arel_value; target = AArch64 arel_target } m
    in
    Pmap.fold add relocs IMap.empty

let sub rels off len =
  rels
  |> IMap.to_list
  |> List.filter_map (fun (pos, rel) -> if off <= pos && pos < off + len then Some (pos-off, rel) else None)
  |> IMap.of_list

let rec pp_exp = Pp.(
  function
  | Section s -> !^s
  | Const x -> int x
  | BinOp (a, Add, b) -> !^"(" ^^ pp_exp a ^^ !^"+" ^^ pp_exp b ^^ !^")"
  | BinOp (a, Sub, b) -> !^"(" ^^ pp_exp a ^^ !^"-" ^^ pp_exp b ^^ !^")"
  | BinOp (a, And, b) -> !^"(" ^^ pp_exp a ^^ !^"&" ^^ pp_exp b ^^ !^")"
  | UnOp (Not, b) -> !^"(" ^^ !^"~" ^^ pp_exp b ^^ !^")"
  | Mask (x, a, b) -> pp_exp x ^^ !^"[" ^^ int a ^^ !^":" ^^ int b ^^ !^"]"
)

let pp_rel rel =
  let target = match rel.target with
  | AArch64 Abi_aarch64_symbolic_relocation.Data640 -> "Data64"
  | AArch64 Abi_aarch64_symbolic_relocation.Data320 -> "Data32"
  | AArch64 Abi_aarch64_symbolic_relocation.ADD -> "ADD"
  | AArch64 Abi_aarch64_symbolic_relocation.ADRP -> "ADRP"
  | AArch64 Abi_aarch64_symbolic_relocation.CALL -> "CALL"
  | AArch64 Abi_aarch64_symbolic_relocation.LDST -> "LDST"
  in
  let expr = pp_exp rel.value in
  Pp.(!^target ^^ !^": " ^^ expr)

let pp rels =
  if IMap.is_empty rels then
    Pp.empty
  else
    Pp.(mapping "relocations" @@ List.map (fun (i, r) -> (hex i, pp_rel r)) (IMap.to_list rels))