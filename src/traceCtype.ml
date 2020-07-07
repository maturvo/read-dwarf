(** This module provide most of the C type inference utility *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open TraceContext
open Fun

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Expressions}

    This section implements all the C type expression inference rules.

    C Types are always option and if any part of an expression is not typed,
    then the result is not typed.
*)

(** A typed {!Trace.exp} *)
type tval = { ctyp : Ctype.t option; exp : Trace.exp }

let get_ctypo tval = tval.ctyp

let get_ctyp tval = tval.ctyp |> Opt.get

(** Output a Machine type of the same size at the original type.
    If [update] has a value, the size if modified by the update value. *)
let machine_of_size ?(update = 0) (typ : Ctype.t) : Ctype.t =
  let size = Ctype.sizeof typ in
  let constexpr = typ.constexpr in
  Ctype.machine ~constexpr (size + update)

let unop (u : Ast.unop) tval : Ctype.t option =
  let open Opt in
  let* typ = tval.ctyp in
  match u with
  | Not | Bvredand | Bvredor -> None
  | Bvneg | Bvnot -> machine_of_size typ |> some
  | Extract (b, a) ->
      debug "Extracting from type %t" PP.(top (opt Ctype.pp) tval.ctyp);
      if (* HACK for adrp: a = 0 && b = Arch.address_size - 1 &&*) Ctype.is_ptr typ then tval.ctyp
      else
        let bitsize = b - a + 1 in
        let constexpr = typ.constexpr in
        if bitsize mod 8 = 0 then Ctype.machine ~constexpr (bitsize / 8) |> some else None
  | ZeroExtend m | SignExtend m ->
      if m mod 8 = 0 then machine_of_size ~update:(m / 8) typ |> some else None

let constexpr_to_int ~ctxt e =
  try
    let vctxt v = expand_var ~ctxt v Ast.unknown |> ConcreteEval.eval_direct in
    e |> ConcreteEval.eval_direct ~ctxt:vctxt |> Value.expect_bv |> BitVec.to_int
  with ConcreteEval.Symbolic ->
    err "Expression %t was typed as constexpr but is not constant" (PP.top Trace.pp_exp e);
    Raise.again ConcreteEval.Symbolic

let binop ~ctxt (b : Ast.no Ast.binop) (tval : tval) (tval' : tval) : Ctype.t option =
  let open Opt in
  let* typ = tval.ctyp and* typ' = tval'.ctyp in
  match b with
  | Eq | Neq | Bvcomp _ -> None
  | Bvarith Bvsub when Ctype.is_ptr typ ->
      if typ'.constexpr then
        let v' = constexpr_to_int ~ctxt tval'.exp in
        Ctype.ptr_update typ (-v') |> some
      else Ctype.ptr_forget typ |> some
  | Bvarith _ ->
      let constexpr = typ.constexpr && typ'.constexpr in
      Ctype.machine ~constexpr (Ctype.sizeof typ) |> some
  | Binmem b -> Ast.destr_binmem b

let manyop ~ctxt (m : Ast.manyop) (tvals : tval list) : Ctype.t option =
  let open Opt in
  match m with
  | And | Or -> None
  | Bvmanyarith Bvadd -> (
      if List.exists (fun tval -> tval.ctyp = None) tvals then None
      else
        let typs = List.map get_ctyp tvals in
        let (ptrs, rest) = List.partition (get_ctyp %> Ctype.is_ptr) tvals in
        Opt.some
        @@
        match ptrs with
        | [] ->
            let constexpr = List.for_all Ctype.is_constexpr typs in
            let size = Ctype.sizeof @@ List.hd typs in
            Ctype.machine ~constexpr size
        | [ptr] ->
            let constexpr = List.for_all (get_ctyp %> Ctype.is_constexpr) rest in
            if constexpr then (
              let v = List.fold_left (fun v tval -> v + constexpr_to_int ~ctxt tval.exp) 0 rest in
              debug "Update by %t" PP.(top shex v);
              let t = Ctype.ptr_update (get_ctyp ptr) v in
              debug "to get %t" PP.(top Ctype.pp t);
              t
            )
            else Ctype.ptr_forget (get_ctyp ptr)
        | _ ->
            warn "Multiple pointers in an addition, returning Machine";
            Ctype.machine Ctype.ptr_size
    )
  | Bvmanyarith _ ->
      let+ typs = map_lift (fun tval -> tval.ctyp) tvals in
      let constexpr = List.for_all Ctype.is_constexpr typs in
      let size = Ctype.sizeof @@ List.hd typs in
      Ctype.machine ~constexpr size
  | Concat -> (
      (* HACK If concatenating a extracted pointer to thing on the right,
         then it stays a pointer.*)
      match List.hd tvals with
      | {
       exp = Unop (Extract (m, n), _, _);
       ctyp = Some ({ unqualified = Ptr { fragment = Global; offset; _ }; _ } as ctyp);
      } -> (
          match offset with
          | Somewhere -> Some ctyp
          | Const off -> (
              try
                let new_int =
                  constexpr_to_int ~ctxt (Ast.Op.concat (List.map (fun t -> t.exp) tvals))
                in
                debug "concat hack: %x = %t" new_int
                  (PP.top Trace.pp_exp (Ast.Op.concat (List.map (fun t -> t.exp) tvals)));
                Some (Ctype.ptr_set ctyp new_int)
              with ConcreteEval.Symbolic -> Ctype.ptr_forget ctyp |> Opt.some
            )
        )
      | _ -> None
    )

(** Stage 1 expression typer *)
let rec expr ~ctxt (exp : Trace.exp) : Ctype.t option =
  let ctyp =
    match exp with
    | Var (Register reg, l) -> State.get_reg ctxt.state reg |> State.Tval.ctyp
    | Var (Read r, l) -> HashVector.get ctxt.mem_reads r |> State.Tval.ctyp
    | Bits (bv, l) ->
        let size = BitVec.size bv in
        if size mod 8 = 0 || size = Arch.address_size then
          Ctype.machine ~constexpr:true (size / 8) |> Opt.some
        else None
    | Bool (_, l) -> None
    | Enum (_, l) -> None
    | Unop (u, e, l) -> expr_tval ~ctxt e |> unop u
    | Binop (b, e, e', l) ->
        let te = expr_tval ~ctxt e in
        let te' = expr_tval ~ctxt e' in
        binop ~ctxt b te te'
    | Manyop (m, el, l) -> List.map (expr_tval ~ctxt) el |> manyop ~ctxt m
    | Ite (c, e, e', l) -> None
    | Bound _ -> .
    | Let _ -> .
  in
  debug "Typing %t with %t" PP.(top Trace.pp_exp exp) PP.(top (opt Ctype.pp) ctyp);
  ctyp

and expr_tval ~ctxt exp =
  let ctyp = expr ~ctxt exp in
  { exp; ctyp }

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Memory Read} *)

let fragment_at ~(dwarf : Dw.t) ~fenv ~size (frag : Ctype.fragment) at : Ctype.t option =
  let open Opt in
  let env = dwarf.tenv in
  match frag with
  | Unknown -> Ctype.machine size |> Opt.some
  | Single t -> Ctype.type_at ~env ~size t at
  | DynArray t ->
      let at = at mod Ctype.sizeof t in
      Ctype.type_at ~env ~size t at
  | DynFragment i ->
      let frag = Fragment.Env.get fenv i in
      let* (typ, off) = Fragment.at_off_opt frag at in
      Ctype.type_at ~env ~size typ off
  | Global -> (
      match Elf.SymTbl.of_addr_with_offset_opt dwarf.elf.symbols at with
      | Some (sym, offset) -> (
          match Hashtbl.find_opt dwarf.vars sym.name with
          | Some v -> Ctype.type_at ~env ~size v.ctype offset
          | None -> Ctype.machine size |> Opt.some
        )
      | None -> Ctype.machine size |> Opt.some
    )

let ptr_deref ~dwarf ~fenv ~size frag (offset : Ctype.offset) : Ctype.t option =
  match offset with
  | Const at -> fragment_at ~dwarf ~fenv ~size frag at
  | Somewhere -> Ctype.machine size |> Opt.some

(** Does the same as {!State.read}, but additionally take care of reading the type from a fragment
    and marking the type of the read variable. *)
let read ~(dwarf : Dw.t) (s : State.t) ?(ptrtype : Ctype.t option) ~addr ~size : State.tval =
  match ptrtype with
  | Some { unqualified = Ptr { fragment; offset; provenance }; _ } ->
      let fenv = s.fenv in
      let bsize = State.Mem.Size.to_bytes size in
      let ctyp = ptr_deref ~dwarf ~fenv ~size:bsize fragment offset in
      let exp = State.read ~provenance ?ctyp s ~addr ~size in
      { exp; ctyp }
  | Some _ ->
      warn "Reading from non-ptr unimplemented for now";
      State.read_noprov s ~addr ~size |> State.Tval.of_exp
  | None -> State.read_noprov s ~addr ~size |> State.Tval.of_exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Memory Write} *)

let fragment_write_at ~(dwarf : Dw.t) ~fenv ~(ctyp : Ctype.t) (frag : Ctype.fragment) at : unit =
  let env = dwarf.tenv in
  match frag with
  | DynFragment i ->
      debug "Writing at %t in %d: %t" (PP.top PP.shex at) i (PP.top Ctype.pp ctyp);
      let original = Fragment.Env.get fenv i in
      let cleared = Fragment.clear original ~pos:at ~len:(Ctype.sizeof ctyp) in
      let newfrag = Fragment.add cleared at ctyp in
      Fragment.Env.set fenv i newfrag
  | _ -> ()

let ptr_write ~dwarf ~fenv ~ctyp frag (offset : Ctype.offset) : unit =
  match offset with Const at -> fragment_write_at ~dwarf ~fenv ~ctyp frag at | Somewhere -> ()

(** Does the same as {!State.write}, but additionally take care of writing the type
    if the write is on a {!Ctype.FreeFragment}.*)
let write ~(dwarf : Dw.t) (s : State.t) ?(ptrtype : Ctype.t option) ~addr ~size
    (value : State.tval) : unit =
  match ptrtype with
  | Some { unqualified = Ptr { fragment; offset; provenance }; _ } ->
      let fenv = s.fenv in
      Opt.iter (fun ctyp -> ptr_write ~dwarf ~fenv ~ctyp fragment offset) value.ctyp;
      State.write ~provenance s ~addr ~size value.exp
  | _ ->
      warn "Writing without provenance";
      State.write_noprov s ~addr ~size value.exp
