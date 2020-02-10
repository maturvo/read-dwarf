(** This module represent a byte sub view on a Bytes.t object.
    It is additional sugar on top of Linksem's Byte_sequence_wrapper *)

include Byte_sequence_wrapper

type t = Byte_sequence_wrapper.byte_sequence

let blit (bs : t) (srcoff : int) (dst : Bytes.t) (dstoff : int) (len : int) =
  if srcoff < 0 || srcoff + len > bs.len then
    raise (Invalid_argument "BytesSeq.blit : out of bounds ")
  else Bytes.blit bs.bytes (bs.start + srcoff) dst dstoff len

(*****************************************************************************)
(*         Getters                                                           *)
(*****************************************************************************)

let gen_get size getter bs i =
  if 0 <= i && i <= bs.len - size then getter bs.bytes (bs.start + i)
  else
    raise
      (Invalid_argument
         (Printf.sprintf "ByteSeq: invalid access of length %d at %d but size is %d" size i
            bs.len))

let get bs i = gen_get 1 Bytes.get bs i

let get16le bs i = gen_get 2 Bytes.get_int16_le bs i

let get16be bs i = gen_get 2 Bytes.get_int16_be bs i

let get32le bs i = gen_get 4 Bytes.get_int32_le bs i

let get32be bs i = gen_get 4 Bytes.get_int32_be bs i

let get64le bs i = gen_get 8 Bytes.get_int64_le bs i

let get64be bs i = gen_get 8 Bytes.get_int64_be bs i

(** Extract a sub range of a byte sequence *)
let sub bs start len : t =
  if start >= 0 && len >= 0 && start + len <= bs.len then
    { bytes = bs.bytes; start = bs.start + start; len }
  else
    raise
      (Invalid_argument
         (Printf.sprintf "ByteSeq.sub at %d of length %d but total size is %d" start len bs.len))

(** This can instantiated to sub_getter 10 to have getter of BytesSeq.t of size 10 *)
let sub_getter len bs start = sub bs start len

(** Take the first i bytes of the sequence *)
let front i bs =
  if i > bs.len || i < 0 then
    raise
      (Invalid_argument
         (Printf.sprintf "Cannot take the first %d bytes of a bytesseq of size %d" i bs.len))
  else { bytes = bs.bytes; start = bs.start; len = i }

(** Take the last i bytes of the sequence *)
let back i bs =
  if i > bs.len || i < 0 then
    raise
      (Invalid_argument
         (Printf.sprintf "Cannot take the last %d bytes of a bytesseq of size %d" i bs.len))
  else { bytes = bs.bytes; start = bs.start + bs.len - i; len = i }

(** tells if a byteseq fits this size (bs.len mod size = 0) *)
let fit size bs = bs.len mod size = 0

(** Trail is the part of the byte seq that do not fit in the regular size pattern *)
let trail size bs = back (bs.len mod size) bs

(*****************************************************************************)
(*         Iteration                                                         *)
(*****************************************************************************)

(* Warning do not handle the end of the byteseq *)

let gen_iter step getter f bs =
  let e = bs.start + bs.len - step in
  let index = ref bs.start in
  while !index <= e do
    f (getter bs.bytes !index);
    index := !index + step
  done

let iter f bs = gen_iter 1 Bytes.get f bs

let iter16le f bs = gen_iter 2 Bytes.get_int16_le f bs

let iter16be f bs = gen_iter 2 Bytes.get_int16_be f bs

let iter32le f bs = gen_iter 4 Bytes.get_int32_le f bs

let iter32be f bs = gen_iter 4 Bytes.get_int32_be f bs

let iter64le f bs = gen_iter 8 Bytes.get_int64_le f bs

let iter64be f bs = gen_iter 8 Bytes.get_int64_be f bs

(*****************************************************************************)
(*         Folding                                                           *)
(*****************************************************************************)

(* Warning do not handle the end of the byteseq *)

let gen_fold_left iterf f a bs =
  let r = ref a in
  iterf (fun x -> r := f !r x) bs;
  !r

let fold_left f a bs = gen_fold_left iter f a bs

let fold_left16le f a bs = gen_fold_left iter16le f a bs

let fold_left16be f a bs = gen_fold_left iter16be f a bs

let fold_left32le f a bs = gen_fold_left iter32le f a bs

let fold_left32be f a bs = gen_fold_left iter32be f a bs

let fold_left64le f a bs = gen_fold_left iter64le f a bs

let fold_left64be f a bs = gen_fold_left iter64be f a bs

(*****************************************************************************)
(*        To list                                                            *)
(*****************************************************************************)

let gen_to_list folder bs = List.rev (folder (fun list i -> i :: list) [] bs)

let to_list bs = gen_to_list fold_left bs

let to_list16le bs = gen_to_list fold_left16le bs

let to_list16be bs = gen_to_list fold_left16be bs

let to_list32le bs = gen_to_list fold_left32le bs

let to_list32be bs = gen_to_list fold_left32be bs

let to_list64le bs = gen_to_list fold_left64le bs

let to_list64be bs = gen_to_list fold_left64be bs

(*****************************************************************************)
(*        Pretty printing                                                    *)
(*****************************************************************************)

let pp bs = bs |> to_char_list |> List.map PP.byte |> PP.separate PP.space

let pp16le bs : PP.document =
  PP.(
    let front = bs |> to_list16le |> List.map hex16 |> separate space in
    if fit 2 bs then front else front ^^ pp @@ trail 2 bs)

let pp16be bs : PP.document =
  PP.(
    let front = bs |> to_list16be |> List.map hex16 |> separate space in
    if fit 2 bs then front else front ^^ pp @@ trail 2 bs)

let pp32le bs : PP.document =
  PP.(
    let front = bs |> to_list32le |> List.map hex32 |> separate space in
    if fit 4 bs then front else front ^^ pp @@ trail 4 bs)

let pp32be bs : PP.document =
  PP.(
    let front = bs |> to_list32be |> List.map hex32 |> separate space in
    if fit 4 bs then front else front ^^ pp @@ trail 4 bs)

let pp64le bs : PP.document =
  PP.(
    let front = bs |> to_list64le |> List.map hex64 |> separate space in
    if fit 8 bs then front else front ^^ pp @@ trail 8 bs)

let pp64be bs : PP.document =
  PP.(
    let front = bs |> to_list64be |> List.map hex64 |> separate space in
    if fit 8 bs then front else front ^^ pp @@ trail 8 bs)