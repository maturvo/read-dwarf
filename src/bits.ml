(* The documentation is in the mli file *)

module Int = IntBits

(* TODO unsafe integer access in bytes (need C code) *)

let check_index length i =
  if i < 0 || i >= length then Raise.inv_arg "Indexing in an int with index %d" i

let check_range length i l =
  if i < 0 || l <= 0 || i + l > length then
    Raise.inv_arg "Range indexing in an int at %d of len %d" i l

let length bf = Bytes.length bf * 8

let create size = Bytes.create ((size + 7) / 8)

let make size b = Bytes.make ((size + 7) / 8) (if b then '\xff' else '\x00')

let unsafe_get bf i =
  let byte = Bytes.unsafe_get bf (i / 8) |> Char.code in
  Int.unsafe_get byte (i mod 8)

let get bf i =
  check_index (length bf) i;
  unsafe_get bf i

let unsafe_set bf i =
  let byte = Bytes.unsafe_get bf (i / 8) |> Char.code in
  let byte = Int.unsafe_set byte (i mod 8) |> Char.chr in
  Bytes.unsafe_set bf (i / 8) byte

let set bf i =
  check_index (length bf) i;
  unsafe_set bf i

let unsafe_clear bf i =
  let byte = Bytes.unsafe_get bf (i / 8) |> Char.code in
  let byte = Int.unsafe_clear byte (i mod 8) |> Char.chr in
  Bytes.unsafe_set bf (i / 8) byte

let clear bf i =
  check_index (length bf) i;
  unsafe_clear bf i

let unsafe_setb bf i b =
  let byte = Bytes.unsafe_get bf (i / 8) |> Char.code in
  let byte = Int.unsafe_setb byte (i mod 8) b |> Char.chr in
  Bytes.unsafe_set bf (i / 8) byte

let setb bf i b =
  check_index (length bf) i;
  unsafe_setb bf i b

(* TODO Do that in C
   TODO Do that with a bytesSeq *)
let rec unsafe_blit_to_int src isrc dest idest len =
  if idest >= Int.length then dest
  else
    let start_bytei = isrc / 8 in
    let end_bytei = (isrc + len - 1) / 8 in
    let isrc_off = isrc mod 8 in
    if start_bytei = end_bytei then
      let byte = Bytes.unsafe_get src start_bytei |> Char.code in
      Int.unsafe_blit byte isrc_off dest idest len
    else if isrc_off != 0 then
      let byte = Bytes.unsafe_get src start_bytei |> Char.code in
      let op_len = 8 - isrc_off in
      let dest = Int.unsafe_blit byte isrc_off dest idest op_len in
      unsafe_blit_to_int src (isrc + op_len) dest (idest + op_len) (len - op_len)
    else
      let byte = Bytes.unsafe_get src start_bytei |> Char.code in
      let dest = Int.unsafe_blit byte 0 dest idest 8 in
      unsafe_blit_to_int src (isrc + 8) dest (idest + 8) (len - 8)

let blit_to_int src isrc dest idest len =
  let src_length = length src in
  check_range src_length isrc len;
  Int.check_range idest len;
  unsafe_blit_to_int src isrc dest idest len

(* TODO do it in C *)
let rec unsafe_blit_of_int src isrc dest idest len =
  if isrc >= Int.length then ()
  else
    let start_bytei = idest / 8 in
    let end_bytei = (idest + len - 1) / 8 in
    let idest_off = idest mod 8 in
    if start_bytei = end_bytei then
      let byte = Bytes.unsafe_get dest start_bytei |> Char.code in
      let new_byte = Int.unsafe_blit src isrc byte idest_off len in
      Bytes.unsafe_set dest start_bytei (Char.chr new_byte)
    else if idest_off != 0 then begin
      let byte = Bytes.unsafe_get dest start_bytei |> Char.code in
      let op_len = 8 - idest_off in
      let new_byte = Int.unsafe_blit src isrc byte idest_off op_len in
      Bytes.unsafe_set dest start_bytei (Char.chr new_byte);
      unsafe_blit_of_int src (isrc + op_len) dest (idest + op_len) (len - op_len)
    end
    else
      let byte = Int.unsafe_sub src isrc 8 in
      Bytes.unsafe_set dest start_bytei (Char.chr byte);
      unsafe_blit_of_int src (isrc + 8) dest (idest + 8) (len - 8)

let blit_of_int src isrc dest idest len =
  let dest_length = length dest in
  check_range dest_length idest len;
  Int.check_range isrc len;
  unsafe_blit_of_int src isrc dest idest len

(* let unsafe_blit src isrc dest idest len = (* TODO *) *)
