open Logs.Logger (struct
  let str = __MODULE__
end)

open Cmdliner
open Config.CommonOpt

module Sums = Exp.Sums
module Typed = Exp.Typed

module Var = struct
  type t = Left of State.var | Right of State.var

  let equal a b = match (a,b) with
  | Left a, Left b -> State.Var.equal a b
  | Right a, Right b -> State.Var.equal a b
  | _ -> false

  let pp = function
  | Left v -> Pp.(!^"L:" ^^ State.Var.pp v)
  | Right v -> Pp.(!^"R:" ^^ State.Var.pp v)

  let ty = function Left v | Right v -> State.Var.ty v

  let hash = Hashtbl.hash

  let of_string s =
    let v = State.Var.of_string @@ String.sub s 2 (String.length s - 2) in
    match String.sub s 0 2 with
    | "L:" -> Left v
    | "R:" -> Right v
    | _ -> Raise.inv_arg "Invalid variable: %s" s
end

module Exp = struct
  include Exp.Make (Var)

  let left : State.Exp.t -> t = Ast.Manip.exp_map_var (fun v -> Var.Left v)

  let right : State.Exp.t -> t = Ast.Manip.exp_map_var (fun v -> Var.Right v)
end

module Z3sim = Z3.Make (Var)

type value_relation =
| Eq
| EqSection of string
| EqPage of string

type expr_rel = State.exp * value_relation * State.exp

type reg_rel = value_relation State.Reg.Map.t

let pp_rel = Pp.(function
| Eq -> !^"Eq"
| EqSection s -> !^"EqSection " ^^ !^s
| EqPage s -> !^"EqPage " ^^ !^s
)

let relation_to_exp (rel:value_relation) (exp1:State.exp) (exp2:State.exp) =
  let modify e =
    match rel with
    | Eq -> e
    | EqSection s -> Typed.(e - State.Exp.of_var (State.Var.Section s))
    | EqPage s -> Typed.(e - concat [extract ~first:12 ~last:63 (State.Exp.of_var (State.Var.Section s)); bits_int ~size:12 0])
  in
  let e1, e2 = modify exp1, modify exp2 in
  Typed.((Exp.left e1) = (Exp.right e2))


let check ?hyps (exp1:State.exp) (exp2:State.exp) (rel:value_relation) =
  relation_to_exp rel exp1 exp2
  |> Z3sim.check_full ?hyps
  |> Option.value ~default:false

let infer_and_check ?hyps (exp1:State.exp) (exp2:State.exp) =
  let sections = ref [] in
  Ast.Manip.exp_iter_var (function State.Var.Section s -> sections := s::!sections | _ -> ()) exp1;
  let to_check = Eq :: List.map (fun s -> EqSection s) !sections @ List.map (fun s -> EqPage s) !sections in
  List.find_opt (check ?hyps exp1 exp2) to_check

let infer_for_state ~(assume:expr_rel list) (s1:State.t) (s2:State.t) =
  let hyps = List.map (fun (e1,r,e2) -> relation_to_exp r e1 e2) assume in
  State.Reg.Map.mapi (fun reg (tval1:State.tval) ->
    let tval2 = State.get_reg s2 reg in
    infer_and_check ~hyps tval1.exp tval2.exp
  ) s1.regs
  (* TODO memory *)

(* let get_api elfname name = 
  base "Running %s in %s" name elfname;
  let dwarf = Dw.of_file elfname in
  let func =
    Dw.get_func_opt ~name dwarf
    |> Option.value_fun ~default:(fun () -> fail "Function %s wasn't found in %s" name elfname)
  in
  Dw.Func.get_api func *)

let value_rel_for_type: Ctype.unqualified -> value_relation = function
| Ctype.Machine _ | Ctype.Cint _ | Ctype.Cbool | Ctype.Enum _ -> Eq
| Ptr { fragment=Ctype.Global s; _ } -> EqSection s
| Ptr { fragment=Ctype.DynFragment i; _ } -> EqSection ("Dyn_"^string_of_int i)
| _ -> Raise.todo()

let infer_from_types (s:State.t) =
  State.Reg.Map.map (fun (r:State.Tval.t) ->
    Option.map (fun (r:Ctype.t) ->
      value_rel_for_type r.unqualified
    ) r.ctyp
  ) s.regs

let expr_rel_of_reg_rel (s1:State.t) (s2:State.t) reg_rel : expr_rel list =
  let bindings = State.Reg.Map.bindings reg_rel in
  List.filter_map (fun (reg, rel) ->
    Option.map (fun rel ->
      State.get_reg_exp s1 reg, rel, State.get_reg_exp s2 reg
    ) rel
  ) bindings

let run elf name =
  let tree = Run.Func.get_state_tree ~elf ~name () in
  let initial_state = tree.state in
  let initial_regs = infer_from_types initial_state in
  let assume = expr_rel_of_reg_rel initial_state initial_state initial_regs in
  State.Tree.iter (fun _ s ->
    let rel = infer_for_state ~assume s s in
    State.Reg.Map.iteri (fun reg rel ->
      Option.iter (fun rel ->
        Printf.printf "%t: %t\n" (Pp.top State.Reg.pp reg) (Pp.top pp_rel rel)
      ) rel
    ) rel
  ) tree


let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(value & pos 1 string "main" & info [] ~docv:"FUNCTION" ~doc)

let term =
  Term.(
    CmdlinerHelper.func_options comopts run
    $ elf $ func)

let info =
  let doc =
    "Simulation relation on relocatable binary"
  in
  Cmd.(info "relsim" ~doc ~exits)

let command = (term, info)
