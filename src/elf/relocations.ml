module IMap = Map.Make (Int)

type target =
  AArch64 of Abi_aarch64_symbolic_relocation.aarch64_relocation_target

type rel = {
  target : target;
  value : Elf_symbolic.symbolic_expression;
}

type t = rel IMap.t

type linksem_t = LinksemRelocatable.rels

let of_linksem: linksem_t -> t = function
| LinksemRelocatable.AArch64 relocs ->
    let add k Elf_symbolic.{ arel_value; arel_target } m =
      IMap.add (Z.to_int k) { value = arel_value; target = AArch64 arel_target } m
    in
    Pmap.fold add relocs IMap.empty

let sub rels off len =
  rels
  |> IMap.to_list
  |> List.filter_map (fun (pos, rel) -> if off <= pos && pos < off + len then Some (pos-off, rel) else None)
  |> IMap.of_list

let pp_rel rel =
  let target = match rel.target with
  | AArch64 Abi_aarch64_symbolic_relocation.Data640 -> "Data64"
  | AArch64 Abi_aarch64_symbolic_relocation.Data320 -> "Data32"
  | AArch64 Abi_aarch64_symbolic_relocation.ADD -> "ADD"
  | AArch64 Abi_aarch64_symbolic_relocation.ADRP -> "ADRP"
  | AArch64 Abi_aarch64_symbolic_relocation.CALL -> "CALL"
  | AArch64 Abi_aarch64_symbolic_relocation.LDST -> "LDST"
  in
  let expr = Elf_symbolic.pp_sym_expr rel.value in
  Pp.(!^target ^^ !^": " ^^ !^expr)

let pp rels =
  if IMap.is_empty rels then
    Pp.empty
  else
    Pp.(mapping "relocations" @@ List.map (fun (i, r) -> (hex i, pp_rel r)) (IMap.to_list rels))