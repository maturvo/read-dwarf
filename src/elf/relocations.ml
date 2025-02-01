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
(* | Mask of (exp * int * int) *)

type assertion =
| Range of int64 * int64
| Alignment of int

type rel = {
  target : target;
  value : exp;
  assertions: assertion list;
  mask : int * int;
}

type t = rel IMap.t

type linksem_t = LinksemRelocatable.rels

let exp_of_linksem =
  let rec value_of_linksem = function
  | Elf_symbolic.Section s -> Section s
  | Elf_symbolic.Const x -> Const (Z.to_int x)
  | Elf_symbolic.BinOp (x, op, y) -> BinOp (value_of_linksem x, op, value_of_linksem y)
  | Elf_symbolic.UnOp (op, x) -> UnOp (op, value_of_linksem x)
  | Elf_symbolic.AssertRange (_, _, _) -> Raise.fail "AssertRange should not occur in value expression"
  | Elf_symbolic.AssertAlignment (_, _) -> Raise.fail "AssertAlignment should not occur in value expression"
  | Elf_symbolic.Mask (_, _, _) -> Raise.fail "AssertRange should not occur in value expression"
  in function
  | Elf_symbolic.Mask (e, hi, lo) ->
      let rec extract_asserts e =
        match e with
        | Elf_symbolic.AssertRange (e, min, max) -> let (e, a) = extract_asserts e in e, Range (Z.to_int64 min, Z.to_int64 max) :: a
        | Elf_symbolic.AssertAlignment (e, bits) -> let (e, a) = extract_asserts e in e, Alignment (Z.to_int bits) :: a
        | e -> e, []
        in
      let e, assertions = extract_asserts e in
      fun target -> {target; assertions; mask = (Z.to_int hi, Z.to_int lo); value = value_of_linksem e}
  | _ -> Raise.fail "Expression does not have Mask in top level"


let of_linksem: linksem_t -> t = function
| LinksemRelocatable.AArch64 relocs ->
    let add k Elf_symbolic.{ arel_value; arel_target } m =
      IMap.add (Z.to_int k) (exp_of_linksem arel_value (AArch64 arel_target)) m
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
)

let pp_target = Pp.(function
| AArch64 Abi_aarch64_symbolic_relocation.Data640 -> !^"Data64"
| AArch64 Abi_aarch64_symbolic_relocation.Data320 -> !^"Data32"
| AArch64 Abi_aarch64_symbolic_relocation.ADD -> !^"ADD"
| AArch64 Abi_aarch64_symbolic_relocation.ADRP -> !^"ADRP"
| AArch64 Abi_aarch64_symbolic_relocation.CALL -> !^"CALL"
| AArch64 Abi_aarch64_symbolic_relocation.LDST b -> !^"LDST" ^^ int (1 lsl b))

let pp_rel rel =
  let hi, lo = rel.mask in
  Pp.(pp_target rel.target ^^ !^": " ^^ pp_exp rel.value ^^ !^"[" ^^ int hi ^^ !^":" ^^ int lo ^^ !^"]")

let pp rels =
  if IMap.is_empty rels then
    Pp.empty
  else
    Pp.(mapping "relocations" @@ List.map (fun (i, r) -> (hex i, pp_rel r)) (IMap.to_list rels))