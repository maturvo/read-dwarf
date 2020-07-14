(** This module defines a new simplified kind of trace to replace Isla traces in the later stages of the instruction processing.

    The traces are even simpler and more easily typable.
    The possible events are in the type {!event} and traces ({!t}) are just list of them.

    Compared to Isla, the concept of reading a register do not exist anymore.
    Instead, a register can be used as a variable in any expression.
    When a register appears in an expression, it represents the value of the register at
    the start of the trace even if that register was written to later.
    This is why trace are {i not} naively concatenable.

    Furthermore, branching do not exist either. Branching instruction are represented by a
    set of trace.

    The important functions are {!of_isla} to convert and Isla traces
    and {!simplify} for simplify traces.
*)

(** This module contains variable used in traces *)
module Var = struct
  (** A trace variable *)
  type t =
    | Register of Reg.t  (** The value of the register at the beginning of the trace *)
    | Read of int * Ast.Size.t  (** The result of that memory reading operation *)

  (** Convert the variable to the string encoding. For parsing infractructure reason,
      the encoding must always contain at least one [:]. *)
  let to_string = function
    | Register r -> Printf.sprintf "reg:%s" (Reg.to_string r)
    | Read (num, size) ->
        if size = Ast.Size.B64 then Printf.sprintf "read:%i" num
        else Printf.sprintf "read:%i:%dbits" num (Ast.Size.to_bits size)

  (** Inverse of {!to_string} *)
  let of_string s =
    match String.split_on_char ':' s with
    | ["reg"; reg] -> Register (Reg.of_string reg)
    | ["read"; num] -> Read (int_of_string num, Ast.Size.B64)
    | ["read"; num; size] ->
        let size = Scanf.sscanf size "%dbits" Ast.Size.of_bits in
        Read (int_of_string num, size)
    | _ -> Raise.inv_arg "%s is not a Trace.Var.t" s

  (** Pretty prints the variable *)
  let pp v = v |> to_string |> PP.string

  let equal = ( = )

  let hash = Hashtbl.hash

  let ty = function
    | Register reg -> Reg.reg_type reg
    | Read (_, size) -> Ast.Ty_BitVec (Ast.Size.to_bits size)

  let of_reg reg = Register reg
end

(** A trace expression. No let bindings, no memory operations *)
module Exp = struct
  include Exp.Make (Var)

  let of_reg reg = Var.of_reg reg |> of_var
end

type exp = Exp.t

type event =
  | WriteReg of { reg : Reg.t; value : exp }
  | ReadMem of { addr : exp; value : int; size : Ast.Size.t }
  | WriteMem of { addr : exp; value : exp; size : Ast.Size.t }
  | Assert of exp

type t = event list

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

(** Pretty print an expression *)
let pp_exp e = PPExp.pp_exp Var.pp e

(** Pretty print an event *)
let pp_event =
  let open PP in
  function
  | WriteReg { reg; value } ->
      dprintf "Write |reg:%s| with " (Reg.to_string reg) ^^ nest 4 (pp_exp value)
  | ReadMem { addr; value; size } ->
      dprintf "Read |read:%d| of %dbits from " value (Ast.Size.to_bits size)
      ^^ nest 4 (pp_exp addr)
  | WriteMem { addr; value; size } ->
      dprintf "Write %dbits at " (Ast.Size.to_bits size)
      ^^ nest 4 (pp_exp addr ^^ !^" with " ^^ pp_exp value)
  | Assert exp -> !^"Assert " ^^ nest 4 (pp_exp exp)

(** Pretty print a trace *)
let pp events = PP.separate_map PP.hardline pp_event events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Isla to Trace conversion }

    This section perform the conversion from Isla trace to the
    traces of this module.

    The conversion is generrally obvious, however there is subtlety: If the Isla
    trace reads a register after having written it, then the read produce the written
    expression instead of just the symbolic value of that register. That why there is
    a [written_registers] parameter to some function of this section.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Throw an error in case of local conversion error.
    Normally a type-checked Isla trace should not fail in this section *)
exception OfIslaError

(** The context mapping Isla variable numbers to trace expression *)
type value_context = exp HashVector.t

(** Get the exression of the variable at the index.
    Throw {!OfIslaError} if the variable is not bound *)
let get_var vc i =
  match HashVector.get_opt vc i with
  | Some exp -> exp
  | None ->
      warn "Could not find the variable v%d" i;
      raise OfIslaError

(** Convert an Isla expression to a [Trace] expression by replacing all Isla variable
    by their value in the context. Throw {!OfIslaError} if the substitution fails *)
let exp_conv_subst (vc : value_context) (exp : Isla.rexp) : exp =
  let vconv i _ = get_var vc i in
  IslaConv.exp_add_type_var_subst vconv exp

(** Convert an {!Isla.valu} in a expression *)
let exp_of_valu l vc : Isla.valu -> exp = function
  | Val_Symbolic i -> get_var vc i
  | Val_Bool b -> ExpTyped.bool b
  | Val_Bits bv -> ExpTyped.bits_smt bv
  | Val_I (int, size) -> ExpTyped.bits_int ~size int
  | Val_Enum (n, a) -> ExpTyped.enum (n, a)
  | valu ->
      Raise.fail "%t Can't convert %t to a trace expression" (PP.tos PP.lrng l)
        (PP.tos Isla.pp_valu valu)

(** Write an expression to an {!Isla.valu} *)
let write_to_valu vc valu exp =
  match valu with Isla.Val_Symbolic i -> HashVector.set vc i exp | _ -> ()

let event_of_isla ~written_registers ~read_counter ~(vc : value_context) :
    Isla.revent -> event option = function
  | Smt (DeclareConst _, _) -> None
  | Smt (DefineConst (i, e), _) ->
      (try HashVector.set vc i (exp_conv_subst vc e) with OfIslaError -> ());
      None
  | Smt (Assert e, _) -> Some (Assert (exp_conv_subst vc e))
  | Smt (DefineEnum _, _) -> None
  | ReadReg (name, al, valu, _) ->
      let string_path = IslaManip.string_of_accessor_list al in
      let valu = IslaManip.valu_get valu string_path in
      let reg = Reg.of_path (name :: string_path) in
      (* If the register was already written, we use that value, otherwise, we read a
         symbolic variable *)
      ( match Hashtbl.find_opt written_registers reg with
      | Some exp -> write_to_valu vc valu exp
      | None -> write_to_valu vc valu (Exp.of_reg reg)
      );
      None
  | WriteReg (name, al, valu, l) ->
      let string_path = IslaManip.string_of_accessor_list al in
      let valu = IslaManip.valu_get valu string_path in
      let reg = Reg.of_path (name :: string_path) in
      let value = exp_of_valu l vc valu in
      Hashtbl.add written_registers reg value;
      Some (WriteReg { reg; value })
  | ReadMem (result, _kind, addr, size, l) ->
      let addr =
        exp_of_valu l vc addr |> ExpTyped.extract ~last:(Arch.address_size - 1) ~first:0
      in
      let size = Ast.Size.of_bytes size in
      let value = Counter.get read_counter in
      write_to_valu vc result (Exp.of_var @@ Var.Read (value, size));
      Some (ReadMem { addr; size; value })
  | WriteMem (_success, _kind, addr, data, size, l) ->
      let addr =
        exp_of_valu l vc addr |> ExpTyped.extract ~last:(Arch.address_size - 1) ~first:0
      in
      let size = Ast.Size.of_bytes size in
      let value = exp_of_valu l vc data in
      Some (WriteMem { addr; size; value })
  | Cycle _ -> None
  | Branch _ -> None
  | BranchAddress _ -> None
  | WakeRequest _ -> None
  | MarkReg _ -> None
  | SleepRequest _ -> None
  | Sleeping _ -> None
  | Instr _ -> None
  | Barrier _ -> None
  | CacheOp _ -> None

(** Top level function to convert an isla trace to one of this module *)
let of_isla (Trace events : Isla.rtrc) : t =
  let written_registers = Hashtbl.create 10 in
  let read_counter = Counter.make 0 in
  let vc = HashVector.empty () in
  List.filter_map (event_of_isla ~written_registers ~read_counter ~vc) events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Trace simplification } *)

(** A instance of {!Z3.ContextCounter}. *)
module TraceSimpContext = Z3.ContextCounter (struct
  let str = "Trace simplification"
end)

module Z3Tr = Z3.Make (Var)

(** Simplify a trace by using Z3. Perform both local expression simplification and
    global assertion removal (when an assertion is always true) *)
let simplify events =
  let serv = Z3.ensure_started_get () in
  let exp_simplify exp : exp = Z3Tr.simplify serv exp in
  let event_simplify = function
    | WriteReg wr -> Some (WriteReg { wr with value = exp_simplify wr.value })
    | ReadMem rm ->
        Z3Tr.declare_var_always serv (Read (rm.value, rm.size));
        Some (ReadMem { rm with addr = exp_simplify rm.addr })
    | WriteMem wm ->
        Some (WriteMem { wm with addr = exp_simplify wm.addr; value = exp_simplify wm.value })
    | Assert exp -> (
        let nexp = exp_simplify exp in
        match Z3Tr.check_both serv nexp with
        | Some true -> None
        | Some false -> Raise.fail "TODO implement trace deletion in that case"
        | _ ->
            Z3Tr.send_assert serv nexp;
            Some (Assert nexp)
      )
  in
  TraceSimpContext.openc ();
  (* declare all registers *)
  Reg.iter (fun _ reg _ -> Z3Tr.declare_var_always serv (Register reg));
  let events = List.filter_map event_simplify events in
  TraceSimpContext.closec ();
  events
