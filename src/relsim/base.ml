(* open Logs.Logger (struct
  let str = __MODULE__
end)

module Sums = Exp.Sums
module Typed = Exp.Typed

module Var = struct
    (** The type of variables *)
  type t = Left of State.var | Right of State.var

  let equal a b = match (a,b) with
  | Left a, Left b -> State.Var.equal a b
  | Right a, Right b -> State.Var.equal a b
  | _ -> false

  let pp = function
  | Left v -> Pp.(!^"L:" ^^ State.Var.pp v)
  | Right v -> Pp.(!^"R:" ^^ State.Var.pp v)

  (** Get the type of the variable *)
  let ty = function Left v | Right v -> State.Var.ty v

  let hash = Hashtbl.hash

  let of_string = State.Var.of_string (*TODO*)
end

module Exp = struct
  include Exp.Make (Var)

  let left : State.Exp.t -> t = Ast.Manip.exp_map_var (fun v -> Var.Left v)

  let right : State.Exp.t -> t = Ast.Manip.exp_map_var (fun v -> Var.Right v)
end

type sem_type =
  | Value of int
  | Ptr of sem_type

type mem_rel = (State.Exp.t * State.Exp.t * sem_type) list

let rec sem_type_of_ctype Ctype.{unqualified; _} =
  match unqualified with
  | Machine b -> Value b
  | Cint { size; _ } -> Value size
  | Cbool -> Value 1
  | Ptr { fragment=Ctype.DynArray t; _ } -> Ptr (sem_type_of_ctype t)
  | _ -> Raise.todo()

let mem_rel_of_dwarf (dw: (Dw.Var.t * Dw.Var.t) list) : mem_rel =
  List.map (fun ((v1: Dw.Var.t), (v2: Dw.Var.t)) ->
    let addr1 = match v1.locs with
    | [_, Global a] -> 
      a |> Elf.SymTable.to_addr_offset 
        |> State.Exp.of_address ~size:Arch.address_size
        (* |> Ast.Manip.exp_map_var (fun x -> Var.Left x) *)
    | _ -> Raise.todo()
    in
    let addr2 = match v2.locs with
    | [_, Global a] -> 
      a |> Elf.SymTable.to_addr_offset 
        |> State.Exp.of_address ~size:Arch.address_size
        (* |> Ast.Manip.exp_map_var (fun x -> Var.Right x) *)
    | _ -> Raise.todo()
    in
    let stp = sem_type_of_ctype v1.ctype in
    (addr1, addr2, stp)
  ) dw

type rel = mem_rel * Exp.t list

type event = State.Mem.Fragment.Event.t
type block = State.Mem.Fragment.Block.t

let type_at (mem_rel:mem_rel) (block1:block) (block2:block) =
  List.find_map (fun (e1,e2,t) ->
    let (sym1, off1) = Sums.split_concrete e1 in
    let (sym2, off2) = Sums.split_concrete e2 in
    if (BitVec.to_int off1 == block1.offset && BitVec.to_int off2 == block2.offset
        && Option.equal State.Exp.equal sym1 block1.base
        && Option.equal State.Exp.equal sym2 block2.base)
    then
      Some t
    else
      None
  ) mem_rel

module Z3sim = Z3.Make (Var)

let update_rel ((mem, asserts):rel) (e1: event) (e2: event) : rel =
  (* TODO check sizes *)
  match e1, e2 with
  | (Read (block1, v1), Read (block2, v2)) -> (
      match type_at mem block1 block2 with
      | Some (Value _) -> mem, Typed.(Exp.of_var (Left v1) = Exp.of_var (Right v2))::asserts
      | Some (Ptr t) -> (State.Exp.of_var v1, State.Exp.of_var v2, t)::mem, asserts
      | None -> mem, asserts
    )
  | (Write (block1, e1), Write (block2, e2)) -> (
      (match type_at mem block1 block2 with
      | Some (Value _) -> Z3
      | Some (Ptr t) -> Raise.todo() (* Check (e1, e2, t) in mem *)
      | None -> Raise.fail "simrel failed");
      mem, asserts
    )
  | _ -> Raise.fail "simrel failed"


let verify (st1:State.t) (st2:State.t) (dw:(Dw.Var.t * Dw.Var.t) list) =
  let mem_rel = mem_rel_of_dwarf dw in
  Raise.todo()

  
 *)
