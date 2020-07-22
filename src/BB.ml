(** This module provide code to manipulate basic block and run them *)

open Logs.Logger (struct
  let str = __MODULE__
end)

type trc = Trace.t

type state = State.t

(** Type of a basic block.

    The main part is the traces of all the non-branching instruction

    TODO: The branch part are all the possible trace of the last, branching instruction.
    If the list is empty, there is no such last instruction
*)
type t = { main : trc array (* TODO: branch : trc list *) }

(** Take a binary block and call isla on all the instruction to get traces
    Also does the typing of traces for register discovery.
    TODO Support variable length instructions
*)
let from_binary (code : BytesSeq.t) : t =
  let num = BytesSeq.length code / 4 in
  (* TODO fix fixed size instructions *)
  if BytesSeq.length code != num * 4 then
    failwith "BB.from_binary: The specified range cuts an instruction";
  let process (code : BytesSeq.t) : trc =
    let get_normal : Isla.rtrc list -> trc = function
      | [] -> failwith "BB.from_binary: no normal path"
      | [trc] ->
          IslaType.type_trc trc |> ignore;
          Trace.of_isla trc
      | _ -> failwith "BB.from_binary: Multiple path i.e. branching instruction"
    in
    code |> IslaCache.get_traces |> get_normal
  in
  let main = code |> BytesSeq.to_listbs ~len:4 |> List.map process |> Array.of_list in
  { main }

(* Sequence of the second test:
mpool.c:116.6  (mpool_fini) 40012240:  37000049  tbnz
mpool.c:117.3  (mpool_fini) 40012244:  14000038  b
*)

(*$T from_binary
     (try ignore @@ from_binary @@ BytesSeq.of_hex "000"; false with Failure _ ->  true)
     (try ignore @@ from_binary @@ BytesSeq.of_hex ("37000049" ^ "14000038"); false with Failure _ -> true)
*)

(** Simplifies the traces in the basic block *)
let simplify_mut (bb : t) = Array.map_mut Trace.simplify bb.main

(** Run a linear basic block on a state by mutation.

    If [env] is provided, the run is typed.
*)
let run_mut ?dwarf state (bb : t) : unit = Array.iter (TraceRun.trace_mut ?dwarf state) bb.main

(** Run a linear basic block on a trace and return a new state

    If [env] is provided, the run is typed.
*)
let run ?dwarf start (bb : t) : state =
  let state = State.copy start in
  run_mut ?dwarf state bb;
  State.lock state;
  state

(** Pretty print the basic block (The traces) *)
let pp (bb : t) = bb.main |> Array.to_list |> PP.(separate_map (hardline ^^ hardline) Trace.pp)
