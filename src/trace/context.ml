(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module provide the type for a context to run a trace

    Any information that should be required to run a trace but is not part of the state itself
    should be added here
*)

module SMap = Map.Make (String)

(** The context to run a trace *)
type t = {
  reg_writes : (State.Reg.t * State.tval) Vec.t;  (** Stores the delayed register writes *)
  mem_reads : State.tval HashVector.t;  (** Stores the result of memory reads *)
  state : State.t;
  segments : State.exp SMap.t;
  dwarf : Dw.t option;  (** Optionally DWARF information. If present, typing is enabled *)
}

let rec exp_of_relocation: Elf.Relocations.exp -> State.exp = 
  let f = exp_of_relocation in function
  | Section s -> State.Exp.of_var (State.Var.Section s) (* TODO put the actual value there, size? *)
  | Const x -> Exp.Typed.bits (BitVec.of_int x ~size:64) (* TODO size? *)
  | BinOp (a, Add, b) -> Exp.Typed.(f a + f b)
  | BinOp (a, Sub, b) -> Exp.Typed.(f a - f b)
  | BinOp (a, And, b) -> Exp.Typed.manyop (AstGen.Ott.Bvmanyarith AstGen.Ott.Bvand) [f a; f b]
  | UnOp (Not, b) -> Exp.Typed.unop AstGen.Ott.Bvnot (f b)
  | Mask (x, last, first) -> Exp.Typed.extract ~last ~first (f x)

(** Build a {!context} from a state *)
let make_context ?dwarf ?segments_map state =
  let reg_writes = Vec.empty () in
  let mem_reads = HashVector.empty () in
  let segments = segments_map |> Option.value ~default:SMap.empty |> SMap.map exp_of_relocation in
  { state; reg_writes; mem_reads; dwarf; segments }

(** Expand a Trace variable to a State expression, using the context *)
let expand_var ~(ctxt : t) (v : Base.Var.t) (a : Ast.no Ast.ty) : State.exp =
  assert (Base.Var.ty v = a);
  match v with
  | Register reg -> State.get_reg_exp ctxt.state reg
  | NonDet (i, _) | Read (i, _) -> (HashVector.get ctxt.mem_reads i).exp (* TODO is the NonDet case correct *)
  | Segment (name, _) -> SMap.find name ctxt.segments (*TODO put the actual value there*)
  (* | Segment (name, sz) -> Exp.Typed.extract ~first:0 ~last:(sz-1) (State.Exp.of_var (State.Var.Section name)) TODO put the actual value there *)

(** Tell if typing should enabled with this context *)
let typing_enabled ~(ctxt : t) = ctxt.dwarf <> None
