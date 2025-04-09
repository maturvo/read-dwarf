type t = Sym_ocaml.Num.t

let pp x = x |> Dwarf.pphex_sym |> Pp.string

let to_z x = Sym_ocaml.Num.to_num x

let to_int x = Z.to_int @@ to_z x

let of_int x = Sym_ocaml.Num.Absolute (Z.of_int x)
let of_int64 x = Sym_ocaml.Num.Absolute (Z.of_int64 x)

let equal = Sym_ocaml.Num.equal

let max_addr = Z.(shift_left (of_int 1) 64 - (of_int 1))

let min_addr = Z.of_int 0

(* TODO very hacky *)
let less x y = match (x, y) with
| (Sym_ocaml.Num.Absolute x, Sym_ocaml.Num.Offset (_, y)) when Nat_big_num.less x y -> true
| (Sym_ocaml.Num.Absolute x, Sym_ocaml.Num.Offset (_,_)) when Nat_big_num.greater_equal x max_addr -> false
| (Sym_ocaml.Num.Offset (_,_), Sym_ocaml.Num.Absolute y) when Nat_big_num.less max_addr y -> true
| (Sym_ocaml.Num.Offset (_, x), Sym_ocaml.Num.Absolute y) when Nat_big_num.greater_equal x y -> false
| _ -> Sym_ocaml.Num.comp Nat_big_num.less x y
let less_equal = Sym_ocaml.Num.less_equal
let greater = Sym_ocaml.Num.greater
let greater_equal = Sym_ocaml.Num.greater_equal
let compare = Sym_ocaml.Num.compare

let to_string = Sym_ocaml.Num.ppf Z.to_string

let sub = Sym_ocaml.Num.sub

let add = Sym_ocaml.Num.add

let mul = Sym_ocaml.Num.mul

let pow_int_positive x y = Sym_ocaml.Num.Absolute (Nat_big_num.pow_int_positive x y)

let shift_left x s = Sym_ocaml.Num.map (fun x -> Nat_big_num.shift_left x s) x
let shift_right x s = Sym_ocaml.Num.map (fun x -> Nat_big_num.shift_right x s) x
let modulus = Sym_ocaml.Num.map2 Nat_big_num.modulus

let in_range first last x = match (first, last, x) with
| (Sym_ocaml.Num.Absolute f, Sym_ocaml.Num.Absolute l, Sym_ocaml.Num.Absolute x) -> Nat_big_num.less_equal f x && Nat_big_num.less_equal x l
| (Sym_ocaml.Num.Offset (s1, f), Sym_ocaml.Num.Offset (s2, l), Sym_ocaml.Num.Offset (s, x)) when s1 = s2 ->
  s1 = s && Nat_big_num.less_equal f x && Nat_big_num.less_equal x l (* TODO kinda hacky *)
| _ -> Raise.fail "Can't determine if %t is in range [%t,%t]" (Pp.tos pp x) (Pp.tos pp first) (Pp.tos pp last)