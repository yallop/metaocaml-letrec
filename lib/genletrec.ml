(*
 * Copyright (c) 2018 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

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
      | _ -> Attr_support.mkletrec (List.map binding bindings) e

  let underef : expression -> expression =
    let rec m = { default_mapper with
                  expr = fun map e -> 
                    match e with
                    | { pexp_desc =
                          Pexp_apply (_, [_, ({pexp_desc=Pexp_ident _} as e)]);
                        pexp_attributes=ats }
                      when Attr_support.has_attr "letrec:deref" ats ->
                      {e with pexp_attributes = Attr_support.remove_attr "letrec:deref" ats}
                    | e -> default_mapper.expr map e
                } in
    m.expr m

  type letrec_expression =
      Init of expression (* let x = ref dummy in e *)
    | Set of string * expression * expression (* x := rhs; e *)
    | Body of expression

  let rec classify_expression : expression -> letrec_expression = function
    | { pexp_attributes=ats } as e
      when Attr_support.has_attr "letrec:body" ats ->
      Body {e with pexp_attributes = Attr_support.remove_attr "letrec:body" ats}

    | {pexp_desc=Pexp_let (_, [_], e'); pexp_attributes=ats}
      when Attr_support.has_attr "letrec:var" ats ->
      Init e'
    | {pexp_desc = 
         Pexp_sequence ({pexp_desc=Pexp_apply
                             (_, [(_,{pexp_desc=Pexp_ident
                                          {txt=Longident.Lident x}});
                                  (_,rhs)]);
                         pexp_attributes=ats},
                        e')}
      when Attr_support.has_attr "letrec:set" ats ->
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

open Delimcc

type req = 
  | Done
  | MakeSlot: ('a ref code * 'a code -> req) -> req
  | SetSlot : ('a ref code * 'a code) * (unit -> req) -> req

type locus_t = req prompt

let from_option = function Some x -> x | None -> failwith "fromoption";;
let read_answer r = let v = from_option !r in r := None; v

let genletrec_locus : type a. (locus_t -> a code) -> a code
  = fun body ->
  let p = new_prompt () in
  let r = ref None in
  let rec handle = function
    | MakeSlot k ->
       Attr_support.add_attr "letrec:var"
       .< let x = Obj.magic () (* ref (fun _ -> assert false) *)
              in .~(handle (k (.< x >.,
                               Attr_support.add_attr "letrec:deref" .<!x>.))) >.
    | SetSlot ((lhs, rhs), k) ->
       let set = Attr_support.add_attr "letrec:set" .< .~lhs := .~rhs >. in
       .< (.~set ; .~(handle (k ()) )) >.
    | Done ->
       Attr_support.add_attr "letrec:body" (read_answer r)
    in
    Rewrite.typed (handle (push_prompt p (fun () ->
    (r := Some (body p); Done))))

let genletrec : type a b. locus_t -> (a code -> a code) -> a code
  = fun p f ->
      let lhs, use = shift p (fun k -> MakeSlot k) in
      let () = shift p (fun k -> SetSlot ((lhs, f use), k)) in
      use
