type t = Z.t Dwarf.sym0

let to_int x = Z.to_int @@ Dwarf.sym_unwrap x "to_int"