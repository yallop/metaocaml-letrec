(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


(* TODO: compute cliques so that 'ack 3' generates

      let f4 n = n + 1 in

      let rec f3 n = if n = 0 then f4 1 else f4 (f3 (n - 1)) in

      let rec f2 n = if n = 0 then f3 1 else f3 (f2 (n - 1)) in

      let rec f1 n = if n = 0 then f2 1 else f2 (f1 (n - 1)) in

       in f1
*)

(* Code is defined in 4.04.0+BER as
     type code_repr = 
       Code of flvars * Parsetree.expression

   and in 4.03.0+effects-ber as

     type code_repr =
         Code of string loc heap * Parsetree.expression

   I think in both cases we can safely ignore the first component.

   Question in both cases: what happens if we genlet an expression
   that refers to a rec-bound variable?  I *think* that the genlet
   returns the variable immediately when executed.
*)

let add_attr (type a) s (c : a code) : a code =
  let h, e = Obj.magic c in
  Obj.magic (h, Ast_helper.Exp.attr e
               (Location.mknoloc s, Parsetree.PStr []))

let has_attr name attrs =
  List.exists (fun ({Asttypes.txt},_) -> txt = name) attrs

let remove_attr name attrs =
  snd (List.partition (fun ({Asttypes.txt},_) -> txt = name) attrs)

(* Rewrite:

   let xᵢ[@letrec:var] = ref dummy in
   (xᵢ := eᵢ)[@letrec:set];
   e[@letrec:body]
   ↝
   let rec xᵢ = eᵢ[!xᵢ:=xᵢ] in e[!xᵢ:=xᵢ]
*)
module Rewrite : sig
  val typed : 'a code -> 'a code
end =
struct
  open Ast_mapper
  open Asttypes
  open Parsetree

  let letrec : (string * expression) list -> expression -> expression =
    fun bindings e ->
      let binding (x, e) =
        { pvb_pat = Ast_helper.Pat.var (Location.mknoloc x);
          pvb_expr = e;
          pvb_attributes = [];
          pvb_loc = Location.none } in
      match bindings with
      | [] -> e
      | _ ->
        { pexp_desc = Pexp_let (Recursive, List.map binding bindings, e);
          pexp_loc = Location.none;
          pexp_attributes = [] }

  let underef : expression -> expression =
    let rec m = { default_mapper with
                  expr = fun map e -> 
                    match e with
                    | { pexp_desc =
                          Pexp_apply (_, [_, ({pexp_desc=Pexp_ident _} as e)]);
                        pexp_attributes=ats }
                      when has_attr "letrec:deref" ats ->
                      {e with pexp_attributes = remove_attr "letrec:deref" ats}
                    | e -> default_mapper.expr map e
                } in
    m.expr m

  type letrec_expression =
      Init of expression (* let x = ref dummy in e *)
    | Set of string * expression * expression (* x := rhs; e *)
    | Body of expression

  let rec classify_expression : expression -> letrec_expression = function
    | { pexp_attributes=ats } as e
      when has_attr "letrec:body" ats ->
      Body {e with pexp_attributes = remove_attr "letrec:body" ats}

    | {pexp_desc=Pexp_let (_, [_], e'); pexp_attributes=ats}
      when has_attr "letrec:var" ats ->
      Init e'
    | {pexp_desc = 
         Pexp_sequence ({pexp_desc=Pexp_apply
                             (_, [(_,{pexp_desc=Pexp_ident
                                          {txt=Longident.Lident x}});
                                  (_,rhs)]);
                         pexp_attributes=ats},
                        e')}
      when has_attr "letrec:set" ats ->
      Set (x, rhs, e')
    | _ -> failwith "translation failure"

  let rec untyped' : (string * expression) list -> expression -> expression =
    fun binds exp -> match classify_expression exp with
        Init e -> untyped' binds e
      | Set (x, rhs, e) -> untyped' ((x,underef rhs) :: binds) e
      | Body e -> letrec binds (underef e)

  let untyped : expression -> expression =
    fun e -> untyped' [] e

  let typed : type a. a code -> a code =
    fun c -> let x, y = Obj.magic c in Obj.magic (x, untyped y)
end

type (_,_) eql = Refl : ('a,'a) eql

module type SYMBOL = sig
  type _ t
  val eql : 'a t -> 'b t -> ('a, 'b) eql option
end

module type S = sig
  type 'a sym
  type resolve = { resolve: 'a. 'a sym -> 'a code }
  type rhs = { rhs: 'a.resolve -> 'a sym -> 'a code }

  val letrec : rhs -> (resolve -> 'b code) -> 'b code
end

module Make(Sym: SYMBOL) : S with type 'a sym = 'a Sym.t =
struct
  open Delimcc

  type 'a sym = 'a Sym.t
  type resolve = { resolve: 'a. 'a sym -> 'a code }
  type rhs = { rhs: 'a.resolve -> 'a sym -> 'a code }

  type table =
    Nil : table
  | Cons : 'a sym * 'a code * table -> table
  
  type 'a req = 
    | Done: 'a -> 'a req
    | MakeSlot': ('a ref code * 'a code -> 'b req) -> 'b req
    | SetSlot' : ('a ref code * 'a code) * (unit -> 'b req) -> 'b req

  let make_slot' : 'a 'b. 'b req prompt -> 'a ref code * 'a code =
    fun p  -> shift0 p (fun k -> MakeSlot' k)

  let set_slot': 'a 'b. 'b req prompt -> ('a ref code * 'a code) -> unit =
    fun p rx -> shift0 p (fun k -> SetSlot' (rx, k))

  let rec assoc : type a. a sym -> table -> a code =
    fun sym table -> match table with
        Nil -> raise Not_found
      | Cons (k, v, xs) ->
        begin match Sym.eql k sym with
            Some Refl -> v
          | None -> assoc sym xs
        end

  let push : type a. a sym -> a code -> table ref -> unit =
    fun sym code table -> table := Cons (sym, code, !table)

  let insert : type a b. b req prompt -> a sym -> table ref -> (unit -> a code) -> a code =
    fun prompt sym table k ->
      try assoc sym !table with
      | Not_found ->
        let lhs, use = make_slot' prompt in
        push sym use table;
        set_slot' prompt (lhs, k ()); use

  let letrec rhs body =
    let p = new_prompt () in
    let table = ref Nil in
    let rec resolve =
      { resolve = fun sym ->
            insert p sym table @@ fun () ->
            rhs.rhs resolve sym }
    in
    let rec handle = function
      | MakeSlot' k ->
        add_attr "letrec:var"
          .< let x = Obj.magic () (* ref (fun _ -> assert false) *)
              in .~(handle (k (.< x >.,
                               add_attr "letrec:deref" .<!x>.))) >.
      | SetSlot' ((lhs, rhs), k) ->
        let set = add_attr "letrec:set" .< .~lhs := .~rhs >. in
          .< (.~set ; .~(handle (k ()) )) >.
      | Done e ->
        add_attr "letrec:body" e
  in
  Rewrite.typed (handle (push_prompt p @@ fun () ->
                         Done (body resolve)))
end

(* Define a simple 'letrec' for monomorphic recursion
   in terms of the more general implementation along with
   polymorphic equality *)
let letrec  : type a b c.
  ((a -> b code) -> a -> b code) ->
  ((a -> b code) -> c code) -> c code =
  let module N = struct type _ t = T : a -> b t  end in
  let module Sym : SYMBOL with type 'a t = 'a N.t =
  struct
    type 'a t = 'a N.t
    (* Use Pervasives.(=) *)
    let eql : type a b. a t -> b t -> (a, b) eql option =
      fun (N.T x) (N.T y) -> match x = y with
        | true -> Some Refl
        | false -> None
  end in
  fun rhs body ->
    let module R = Make(Sym) in
    let resolve r sym = r.R.resolve (N.T sym) in
    let rhs (type d) r (N.T (sym : a) : d R.sym) : d code =
      rhs (resolve r) sym in 
    R.letrec {R.rhs} (fun r -> body (resolve r))
