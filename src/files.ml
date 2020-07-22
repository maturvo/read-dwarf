(** This module provides simplified file management and some channel interaction function.

    The functions {!read} and {!write} are about dealing with a whole file at once
    without caring about opening or closing it.
*)

(** The type of a reader that read an object from a channel.
    Function like [input_*] in
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html#2_Generalinputfunctions}[Stdlib]}*)
type 'a reader = in_channel -> 'a

(** The type of a writer of 'a.
    Function named [output_*] in
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html#2_Generaloutputfunctions}[Stdlib]} *)
type 'a writer = out_channel -> 'a -> unit

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 IO functions } *)

(** Double the size of a bytes object (technical function) *)
let double_byte b = Bytes.extend b 0 (Bytes.length b)

(** Read the whole channel in a bytes *)
let input_bytes (i : in_channel) : bytes =
  let rec read_all_bytes i b pos len =
    let _ = assert (pos + len = Bytes.length b) in
    let res = input i b pos len in
    match res with
    | 0 -> Bytes.sub b 0 pos
    | x when x = len -> read_all_bytes i (double_byte b) (Bytes.length b) (Bytes.length b)
    | _ -> read_all_bytes i b (pos + res) (len - res)
  in
  read_all_bytes i (Bytes.create 100) 0 100

(** Read the whole channel in a string *)
let input_string (i : in_channel) : string = input_bytes i |> Bytes.unsafe_to_string

(** Reads a S-expression from the input, line by line.
    When the sexp finishes, there should be nothing else on the line i.e.
    the last closing parenthesis should be followed by a new line.*)
let input_sexp (i : in_channel) : string =
  let line0 = input_line i in
  if String.contains line0 '(' then begin
    let pnum = ref 0 in
    let count = String.iter (function '(' -> incr pnum | ')' -> decr pnum | _ -> ()) in
    count line0;
    if !pnum = 0 then line0
    else
      let buf = Buffer.create (2 * String.length line0) in
      Buffer.add_string buf line0;
      while !pnum > 0 do
        let line = input_line i in
        count line;
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      done;
      Buffer.contents buf
  end
  else line0

(** Try the reader until it fails with [End_of_file] and then build the list of all the
    successfully read objects in order.*)
let input_list (elem_reader : 'a reader) (i : in_channel) : 'a list =
  let rec aux acc = try aux (elem_reader i :: acc) with End_of_file -> List.rev acc in
  aux []

(** Output all the element of the list in order with the provided writer *)
let output_list (elem_writer : 'a writer) (o : out_channel) (l : 'a list) =
  List.iter (elem_writer o) l

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Direct file IO } *)

(** Take a reader and a file and read an object from the file using the reader. Text mode *)
let read (reader : 'a reader) (file : string) : 'a =
  let c = open_in file in
  Protect.protect (fun () -> reader c) (fun () -> close_in c)

(** Take a reader and a file and read an object from the file using the reader. Binary mode *)
let read_bin (reader : 'a reader) (file : string) : 'a =
  let c = open_in_bin file in
  Protect.protect (fun () -> reader c) (fun () -> close_in c)

(** Take a writer a file and object and write the object to the file using writer. Text mode *)
let write (writer : 'a writer) (file : string) (obj : 'a) =
  let c = open_out file in
  Protect.protect (fun () -> writer c obj) (fun () -> close_out c)

(** Take a writer fs a file and object and write the object to the file using writer. Binary mode*)
let write_bin (writer : 'a writer) (file : string) (obj : 'a) =
  let c = open_out_bin file in
  Protect.protect (fun () -> writer c obj) (fun () -> close_out c)

(** Return the content of specified file as a string *)
let read_string = read input_string

(** [write_string file cont] write [cont] in [file] which is overwritten if it exists *)
let write_string = write output_string

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 File management } *)

(** Remove a file *)
let remove = Sys.remove

(** Test if a files exists *)
let exists = Sys.file_exists

(** Remove a file at program exit *)
let remove_at_exit s = at_exit (fun () -> try remove s with Sys_error _ -> ())

(** Add [newp] in front of a relative path. If the path is not relative, then it is unchanged *)
let add_to_relative ~newp path =
  if Filename.is_relative path then Filename.concat newp path else path
