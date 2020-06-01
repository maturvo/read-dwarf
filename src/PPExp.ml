(** This module provides a human readable pretty printing for {!Ast} expressions

    If you don't want to bother with details, use [pp_exp] and don't read the rest.

    The precedence are the ones from C/C++ and Ocaml with some tweaks.
    In particular the precedence between bitwise operation and arithmetic operation
    are speparated to parenthesis will always be required between them.

    The order is:
    - Extraction and extension
    - Concatenation
    - unary minus, unary bitwise negation, unary reduction
    - multiplications, divisions, separately shift
    - additions, substractions, separately bitwise operation
    - comparisons
    - equality
    - and
    - or
    - ifs

    Unary operator cannot linebreak (but their content can)

    Examples:

      - [-a\[1-3\].2a:6\[z+32\]] which is:
        [(bvneg (concat ((_ extract 3 1) a) ((_ zero_extend 32) #b101010)))]
*)

open PP

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Precedence } *)

(** The operators possible precedence *)
type prec =
  | IF
  | OR
  | AND
  | EQ
  | COMP
  | ADD
  | MUL
  | BITS
  | SHIFTS
  | UNARY
  | CONCAT
  | EXTRACT
  | PARENS

(** Tell a expression of precedence [inner] in an
    expression of precedence [outer] need parenthesis *)
let compat ~outer ~inner =
  let arith o = o = ADD || o = MUL in
  let bits o = o = BITS || o = SHIFTS in
  if (arith outer && bits inner) || (bits outer && bits inner) then false else outer < inner

let prec_unop : Ast.unop -> prec = function
  | Extract _ | ZeroExtend _ | SignExtend _ -> EXTRACT
  | _ -> UNARY

let prec_bvarith : Ast.bvarith -> prec = function
  | Bvnand -> AND
  | Bvnor | Bvxnor -> OR
  | Bvsub -> ADD
  | Bvshl | Bvlshr | Bvashr -> SHIFTS
  | Bvudiv | Bvudivi | Bvsdiv | Bvsdivi | Bvurem | Bvsrem | Bvsmod -> MUL

let prec_binop : Ast.no Ast.binop -> prec = function
  | Eq | Neq -> EQ
  | Bvarith bva -> prec_bvarith bva
  | Bvcomp _ -> COMP
  | Binmem m -> Ast.destr_binmem m

let prec_bvmanyarith : Ast.bvmanyarith -> prec = function
  | Bvand | Bvor | Bvxor -> BITS
  | Bvadd -> ADD
  | Bvmul -> MUL

let prec_manyop : Ast.manyop -> prec = function
  | And -> AND
  | Or -> OR
  | Concat -> CONCAT
  | Bvmanyarith bvma -> prec_bvmanyarith bvma

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Actual Pretty printing } *)

let pp_bits bv =
  (bv |> BitVec.to_string ~base:16 ~force_width:false ~prefix:true |> string)
  ^^ colon
  ^^ (bv |> BitVec.size |> int)

let ppnot = PP.char '!'

let pp_unop (u : Ast.unop) doc =
  match u with
  | Not -> ppnot ^^ doc
  | Bvnot -> tilde ^^ doc
  | Bvneg -> minus ^^ doc
  | Bvredor -> !^"|>" ^^ doc ^^ !^"<|"
  | Bvredand -> !^"&>" ^^ doc ^^ !^"<&"
  | Extract (b, a) -> doc ^^ dprintf "[%d:%d]" a b
  | ZeroExtend m -> doc ^^ dprintf "[z+%d]" m
  | SignExtend m -> doc ^^ dprintf "[s+%d]" m

let sym_bvarith : Ast.bvarith -> document = function
  | Bvnand -> !^"&!&"
  | Bvnor -> !^"|!|"
  | Bvxnor -> !^"^!^"
  | Bvsub -> minus
  | Bvudiv -> !^"u/"
  | Bvudivi -> !^"u/i"
  | Bvsdiv -> !^"s/"
  | Bvsdivi -> !^"s/i"
  | Bvurem -> !^"urem"
  | Bvsrem -> !^"srem"
  | Bvsmod -> !^"smod"
  | Bvshl -> !^"<<"
  | Bvlshr -> !^"l>>"
  | Bvashr -> !^"a>>"

let sym_bvcomp : Ast.bvcomp -> document = function
  | Bvult -> !^"u<"
  | Bvslt -> !^"s<"
  | Bvule -> !^"u<="
  | Bvsle -> !^"s<="
  | Bvuge -> !^"u>="
  | Bvsge -> !^"s>="
  | Bvugt -> !^"u>"
  | Bvsgt -> !^"s>"

let pp_binop (b : Ast.no Ast.binop) doc doc' =
  let bin sym doc doc' = doc ^^ space ^^ sym ^^ nbspace ^^ doc' |> nest 4 |> group in
  match b with
  | Eq -> bin equals doc doc' |> group
  | Neq -> bin !^"!=" doc doc' |> group
  | Bvarith bva -> bin (sym_bvarith bva) doc doc' |> group
  | Bvcomp bvc -> bin (sym_bvcomp bvc) doc doc' |> group
  | Binmem m -> Ast.destr_binmem m

let sym_bvmanyarith : Ast.bvmanyarith -> document = function
  | Bvand -> !^"&"
  | Bvor -> bar
  | Bvxor -> !^"^"
  | Bvadd -> plus
  | Bvmul -> star

let pp_manyop (m : Ast.manyop) docs =
  let many sym docs = separate (space ^^ sym ^^ nbspace) docs |> nest 4 |> group in
  match m with
  | And -> many !^"and" docs
  | Or -> many !^"or" docs
  | Concat -> separate (break 0 ^^ dot) docs |> nest 4 |> group
  | Bvmanyarith bvma -> many (sym_bvmanyarith bvma) docs

let pp_if cond doc doc' =
  prefix 2 1 !^"if" cond ^/^ prefix 2 1 !^"then" doc ^/^ prefix 2 1 !^"else" doc' |> group

(** Pretty print an expression and also returns it's precedence *)
let rec pp_exp_prec ppv : ('a, 'v, Ast.no, Ast.no) Ast.exp -> document * prec =
  let parens_if ~outer (doc, inner) = if compat ~outer ~inner then doc else parens doc in
  function
  | Var (v, _) -> (group (ppv v), PARENS)
  | Bound _ -> .
  | Bits (bv, _) -> (pp_bits bv, PARENS)
  | Bool (b, _) -> (bool b, PARENS)
  | Enum (e, _) -> (Ast.pp_enum e, PARENS)
  | Unop (u, e, _) ->
      let outer = prec_unop u in
      let doc = parens_if ~outer @@ pp_exp_prec ppv e in
      (pp_unop u doc, outer)
  | Binop (b, e, e', _) ->
      let outer = prec_binop b in
      let doc = parens_if ~outer @@ pp_exp_prec ppv e in
      let doc' = parens_if ~outer @@ pp_exp_prec ppv e' in
      (pp_binop b doc doc', outer)
  | Manyop (m, el, _) ->
      let outer = prec_manyop m in
      let docs = List.map (fun e -> parens_if ~outer @@ pp_exp_prec ppv e) el in
      (pp_manyop m docs, outer)
  | Ite (c, e, e', _) ->
      let outer = IF in
      let docc = parens_if ~outer @@ pp_exp_prec ppv c in
      let doc = parens_if ~outer @@ pp_exp_prec ppv e in
      let doc' = parens_if ~outer @@ pp_exp_prec ppv e' in
      (pp_if docc doc doc', outer)
  | Let _ -> .

(** The main function for pretty printing an expression *)
let pp_exp ppv exp = pp_exp_prec ppv exp |> fst