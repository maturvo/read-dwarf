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

type sem_type =
| Value of int
| Ptr of sem_type

type value_relation =
| Eq
| EqSection of string
| EqPage of string
| Indirect of sem_type

let rec pp_sem_type = Pp.(function
| Value w -> !^"Val"^^(int w)
| Ptr typ -> (pp_sem_type typ)^^(!^"*")
)

let pp_rel = Pp.(function
| Eq -> !^"Eq"
| EqSection s -> !^"EqSection " ^^ !^s
| EqPage s -> !^"EqPage " ^^ !^s
| Indirect typ -> !^"Indirect " ^^ (pp_sem_type typ)
)

let rec sem_type_of_type (typ: Ctype.t) : sem_type =
  match typ.unqualified with
  | Ctype.Machine _ | Ctype.Cint _ | Ctype.Cbool | Ctype.Enum _ -> Value (Ctype.sizeof typ)
  | Ptr { fragment=Ctype.DynArray typ'; _ } -> Ptr (sem_type_of_type typ')
  | _ -> Raise.todo()

let value_rel_for_type: Ctype.unqualified -> value_relation = function
| Ctype.Machine _ | Ctype.Cint _ | Ctype.Cbool | Ctype.Enum _ -> Eq
| Ptr { fragment=Ctype.Global s; _ } -> EqSection s
| Ptr { fragment=Ctype.DynFragment i; _ } -> EqSection ("Dyn_"^string_of_int i)
| _ -> Raise.todo()

module ExpRel = struct
  type t = State.exp * value_relation * State.exp

  let to_exp ((exp1, rel, exp2):t) =
    let open Option in
    let modify e =
      match rel with
      | Eq -> e |> some
      | EqSection s -> Typed.(e - State.Exp.of_var (State.Var.Section s)) |> some
      (* TODO this is probably wrong: *)
      | EqPage s -> Typed.(e - concat [extract ~first:12 ~last:63 (State.Exp.of_var (State.Var.Section s)); bits_int ~size:12 0]) |> some
      | Indirect _ -> None
    in
    let+ e1, e2 = lift_pair (modify exp1, modify exp2) in
    Typed.((Exp.left e1) = (Exp.right e2))
  
  let check ?hyps (rel:t) =
    match to_exp rel with
    | None -> false
    | Some e -> e |> Z3sim.check_full ?hyps |> Option.value_fun ~default:Raise.todo
  
  let infer ?hyps (exp1:State.exp) (exp2:State.exp) =
    let sections = ref [] in
    Ast.Manip.exp_iter_var (function State.Var.Section s -> sections := s::!sections | _ -> ()) exp1;
    let to_check = if Typed.get_type exp1 = Ast.Ty_BitVec 64 then
      Eq :: List.map (fun s -> EqSection s) !sections @ List.map (fun s -> EqPage s) !sections
    else
      [Eq]
    in
    List.find_opt (fun r -> check ?hyps (exp1, r, exp2)) to_check
end

module RegRel = struct
  type t = value_relation State.Reg.Map.t
  
  let infer ~(assume:ExpRel.t list) (s1:State.t) (s2:State.t) =
    let hyps = List.filter_map ExpRel.to_exp assume in
    State.Reg.Map.mapi (fun reg (tval1:State.tval) ->
      let tval2 = State.get_reg s2 reg in
      ExpRel.infer ~hyps tval1.exp tval2.exp
    ) s1.regs
  
  let special_regs = ["OSDLR_EL1"; "OSLSR_EL1"; "EDSCR"; "SCR_EL3"]

  let infer_from_types (s:State.t) =
    State.Reg.Map.mapi (fun reg (r:State.Tval.t) ->
      if List.exists ((=) (State.Reg.to_string reg)) special_regs then
        Some Eq
      else
        Option.map (fun (r:Ctype.t) ->
          value_rel_for_type r.unqualified
        ) r.ctyp
    ) s.regs

  let to_exp_rel (s1:State.t) (s2:State.t) reg_rel : ExpRel.t list =
    let bindings = State.Reg.Map.bindings reg_rel in
    List.filter_map (fun (reg, rel) ->
      Option.map (fun rel ->
        State.get_reg_exp s1 reg, rel, State.get_reg_exp s2 reg
      ) rel
    ) bindings
end

module StackRel = struct
  module RelMap = RngMap.Make (struct
    type t = value_relation * int
    let len (_, sz: t) = sz
  end)
  type t = RelMap.t

  module Event = State.Mem.Fragment.Event

  let rec process_trace ~exps ~stack (tr1:Event.t list) (tr2:Event.t list) =
    match tr1, tr2 with
    | [], [] -> (exps, stack)
    | Event.Read (blk1, v1) :: tr1, Event.Read (blk2, v2) :: tr2 ->
      let (exps, stack) = process_trace ~exps ~stack tr1 tr2 in

      if Option.is_some blk1.base || Option.is_some blk2.base then
        Raise.todo();
      if blk1.offset != blk2.offset || blk1.size != blk2.size then
        Raise.fail "traces don't match";
      
      let offset = blk1.offset in
      let size = Ast.Size.to_bytes blk1.size in
      let exps = match RelMap.at_off_opt stack offset with
      | Some((rel, relsz), reloff) ->
        if size != relsz || reloff != 0 then
          Raise.todo();
        (State.Exp.of_var v1, rel, State.Exp.of_var v2)::exps
      | None ->
        warn "No relation for read";
        exps
      in
      (exps, stack)
    | Event.Write (blk1, exp1) :: tr1, Event.Write (blk2, exp2) :: tr2 ->
      let (exps, stack) = process_trace ~exps ~stack tr1 tr2 in

      if Option.is_some blk1.base || Option.is_some blk2.base then
        Raise.todo();
      if blk1.offset != blk2.offset || blk1.size != blk2.size then
        Raise.fail "traces don't match";
      
      let offset = blk1.offset in
      let size = Ast.Size.to_bytes blk1.size in

      let stack = RelMap.clear stack ~pos:offset ~len:size in      
      let hyps = List.filter_map ExpRel.to_exp exps in
      let stack = match ExpRel.infer ~hyps exp1 exp2 with
      | None ->
          warn "Failed to find correspondence (TODO better warn message)";
          stack
      | Some rel ->
          RelMap.add stack offset (rel, size)
      in
      (exps, stack)
    | _ -> Raise.fail "traces don't match"

  let infer ~exps ~prev_stack ~stack_prov (st1:State.t) (st2:State.t) =
    (* Printf.printf "%t\n%t\n" (Pp.top State.pp st1) (Pp.top State.pp st2); *)
    let ((_,mem1), (_,mem2)) = State.Mem.(get_frag st1.mem stack_prov, get_frag st2.mem stack_prov) in
    let (trc1, trc2) = State.Mem.Fragment.(get_trace mem1, get_trace mem2) in
    process_trace ~exps ~stack:prev_stack trc1 trc2
  
  let infer_from_types ~stack_frag (st1:State.t) =
    let frag = Vec.get st1.fenv.frags stack_frag in
    let stack = ref RelMap.empty in
    State.Fragment.iteri (fun off ctype ->
      stack := RelMap.add !stack off (value_rel_for_type ctype.unqualified, Ctype.len ctype)
    ) frag;
    !stack
end

module GlobalRel = struct
  type eq_pair = State.exp * State.exp * sem_type

  type t = eq_pair list

  let find (rel:t) a1 a2 =
    let check_one a1 a2 (a1', a2', typ) =
      Z3sim.check_full Typed.(manyop Ast.And [Exp.left a1= Exp.right a1'; Exp.left a2= Exp.right a2'])
      |> Option.value_fun ~default:Raise.todo
    in
    List.find_map (check_one a1 a2) rel

  let check (rel:t) (pair: eq_pair) =
    let check_one (a1, a2, typ) (a1', a2', typ') =
      typ = typ' && (
        Z3sim.check_full Typed.(manyop Ast.And [Exp.left a1= Exp.right a1'; Exp.left a2= Exp.right a2'])
        |> Option.value_fun ~default:Raise.todo
      )
    in
    List.exists (check_one pair) rel
end


(* let get_api elfname name = 
  base "Running %s in %s" name elfname;
  let dwarf = Dw.of_file elfname in
  let func =
    Dw.get_func_opt ~name dwarf
    |> Option.value_fun ~default:(fun () -> fail "Function %s wasn't found in %s" name elfname)
  in
  Dw.Func.get_api func *)

type ctxt = {
  mem_sim: (State.Id.t*State.Id.t, StackRel.t) Hashtbl.t;
  mutable asserts: ExpRel.t list;
}

let stack_prov = 0(* TODO determine stack_frag automatically *)
let stack_frag = 0(* TODO determine stack_frag automatically *)

let rec checksim ~(ctxt:ctxt) (s1:State.t) (s2:State.t) =
  Hashtbl.find_opt ctxt.mem_sim (s1.id, s2.id)
  |> Option.value_fun ~default:(fun() ->
    let bs1 = Option.value_fail s1.base_state "no base state" in
    let bs2 = Option.value_fail s2.base_state "no base state" in
    let prev_stack = checksim ~ctxt bs1 bs2 in

    let asserts, stack = StackRel.infer ~exps:ctxt.asserts ~prev_stack ~stack_prov s1 s2 in

    let asserts_exp = List.filter_map ExpRel.to_exp asserts in
    let asst1 = List.map Exp.left s1.asserts in
    let asst2 = List.map Exp.right s2.asserts in
    let impl1 = List.for_all Fun.(Z3sim.check_full ~hyps:(asst1@asserts_exp) %> flip Option.value_fail "TODO: Z3 failed") asst2 in
    let impl2 = List.for_all Fun.(Z3sim.check_full ~hyps:(asst2@asserts_exp) %> flip Option.value_fail "TODO: Z3 failed") asst1 in
    if not (impl1 && impl2) then
      failwith "States not equivalent"; (*TODO more info*)

    Hashtbl.add ctxt.mem_sim (s1.id, s2.id) stack;
    ctxt.asserts <- asserts @ ctxt.asserts;
    stack
  )

let run elf name =
  let tree = Run.Func.get_state_tree ~elf ~name () in
  let initial_state = tree.state in

  let initial_regs = RegRel.infer_from_types initial_state in
  
  let initial_stack = StackRel.infer_from_types initial_state ~stack_frag in 
  let ctxt = {
    mem_sim=Hashtbl.create 10;
    asserts=RegRel.to_exp_rel initial_state initial_state initial_regs;
  } in
  Hashtbl.add ctxt.mem_sim (initial_state.id, initial_state.id) initial_stack;
  State.Tree.iter (fun _ s ->
    let stack = checksim ~ctxt s s in
    Printf.printf "-- STATE %t --\n" (Pp.top State.Id.pp s.id);
    StackRel.RelMap.iteri (fun i (rel,_sz) ->
      Printf.printf "SP+%d: %t\n" i (Pp.top pp_rel rel)
    ) stack;
    let rel = RegRel.infer ~assume:ctxt.asserts s s in
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
