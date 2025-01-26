type t = Z.t Dwarf.sym0

let to_int x = Z.to_int @@ Dwarf.sym_unwrap x "to_int"

let of_int x = Dwarf.Absolute (Z.of_int x)

let equal = Dwarf.sym_comp Nat_big_num.equal
let less = Dwarf.sym_comp Nat_big_num.less
let less_equal = Dwarf.sym_comp Nat_big_num.less_equal
let greater = Dwarf.sym_comp Nat_big_num.greater
let greater_equal = Dwarf.sym_comp Nat_big_num.greater_equal

let to_string = Dwarf.pp_sym Z.to_string

let sub x y = match (x, y) with
| (Dwarf.Offset (s, a), Dwarf.Offset (t, b)) when s = t -> Nat_big_num.sub a b
| (Dwarf.Absolute a, Dwarf.Absolute b) -> Nat_big_num.sub a b
| _ -> failwith "Can't compare"