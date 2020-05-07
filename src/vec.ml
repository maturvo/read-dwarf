(* The documentation is in the mli file *)

open Res.Array

type 'a t = 'a Res.Array.t

let length = length

let empty = empty

let map = map

let get = get

let unsafe_get = unsafe_get

let set = set

let unsafe_set = unsafe_set

let copy = copy

let map = map

let mapi = mapi

let iter = iter

let to_list = to_list

let fold_left = fold_left

let add_one = add_one

let remove_one = remove_one

let remove_n = remove_n

let to_array = to_array

let of_array = of_array

(* TODO optimize this *)
let ensure vec size v =
  let len = length vec in
  if size > len then
    for _ = len to size - 1 do
      add_one vec v
    done
  else ()

let resize vec size v =
  let len = length vec in
  if size < len then remove_n vec (len - size) else ensure vec size v

(* TODO optimize this *)
let map2 f veca vecb =
  let arra = to_array veca and arrb = to_array vecb in
  Array.map2 f arra arrb |> of_array

let map_mut f vec =
  let len = length vec in
  for i = 0 to len - 1 do
    unsafe_get vec i |> f |> unsafe_set vec i
  done

let fill_all vec elem =
  let len = length vec in
  for i = 0 to len - 1 do
    unsafe_set vec i elem
  done

let pp conv vec = PP.(!^"vec" ^^ (vec |> to_array |> array conv))

let ppi conv vec = PP.(vec |> to_list |> List.mapi (fun i v -> (int i, conv v)) |> PP.mapping "")