type t = Z.t Dwarf.sym0

let to_int x = Z.to_int @@ Dwarf.sym_unwrap x "to_int"

let of_int x = Dwarf.Absolute (Z.of_int x)
let of_int64 x = Dwarf.Absolute (Z.of_int64 x)

let equal = Dwarf.sym_comp Nat_big_num.equal
let less = Dwarf.sym_comp Nat_big_num.less
let less_equal = Dwarf.sym_comp Nat_big_num.less_equal
let greater = Dwarf.sym_comp Nat_big_num.greater
let greater_equal = Dwarf.sym_comp Nat_big_num.greater_equal
let compare = Dwarf.sym_comp Nat_big_num.compare

let to_string = Dwarf.pp_sym Z.to_string

let sub x y = match (x, y) with
| (Dwarf.Offset (s, a), Dwarf.Offset (t, b)) when s = t -> Dwarf.Absolute (Nat_big_num.sub a b)
| (Dwarf.Absolute a, Dwarf.Absolute b) -> Dwarf.Absolute (Nat_big_num.sub a b)
| _ -> Dwarf.Unknown

let add x y = match (x, y) with
| (Dwarf.Offset (s, a), Dwarf.Absolute b) -> Dwarf.Offset (s, Nat_big_num.add a b)
| (Dwarf.Absolute (a), Dwarf.Offset (s,b)) -> Dwarf.Offset (s, Nat_big_num.add a b)
| (Dwarf.Absolute a, Dwarf.Absolute b) -> Dwarf.Absolute (Nat_big_num.add a b)
| _ -> Dwarf.Unknown

let mul = Dwarf.sym_map2 Nat_big_num.mul

let pow_int_positive x y = Dwarf.Absolute (Nat_big_num.pow_int_positive x y)

let shift_left x s = Dwarf.sym_map (fun x -> Nat_big_num.shift_left x s) x
let shift_right x s = Dwarf.sym_map (fun x -> Nat_big_num.shift_right x s) x
let modulus = Dwarf.sym_map2 Nat_big_num.modulus