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
  type 'a sym = 'a Sym.t
  type resolve = { resolve: 'a. 'a sym -> 'a code }
  type rhs = { rhs: 'a.resolve -> 'a sym -> 'a code }

  type table =
    Nil : table
  | Cons : 'a sym * 'a code * table -> table

  let rec assoc : type a. a sym -> table -> a code =
    fun sym table -> match table with
        Nil -> raise Not_found
      | Cons (k, v, xs) ->
        begin match Sym.eql k sym with
            Some Refl -> v
          | None -> assoc sym xs
        end

  let push sym code table = table := Cons (sym, code, !table)

  let letrec {rhs} body =
    let table = ref Nil in
    Genletrec.genletrec_locus @@ fun locus ->
    let rec resolver = 
      { resolve = fun symbol ->
        try assoc symbol !table with
          Not_found ->
          Genletrec.genletrec locus
            (fun x -> 
              push symbol x table;
              rhs resolver symbol) }
    in
    body resolver
end

(* Define a simple 'letrec' for monomorphic recursion
   in terms of the more general implementation *)
let letrec  : type a b c.
  ?equal:(a -> a -> bool) -> 
  ((a -> b code) -> a -> b code) ->
  ((a -> b code) -> c code) -> c code =
  fun ?(equal=(=)) rhs body ->
  let module N = struct type _ t = T : a -> b t  end in
  let module Sym : SYMBOL with type 'a t = 'a N.t =
  struct
    type 'a t = 'a N.t
    let eql : type a b. a t -> b t -> (a, b) eql option =
      fun (N.T x) (N.T y) -> match equal x y with
        | true -> Some Refl
        | false -> None
  end in
    let module R = Make(Sym) in
    let resolve r sym = r.R.resolve (N.T sym) in
    let rhs (type d) r (N.T (sym : a) : d R.sym) : d code =
      rhs (resolve r) sym in 
    R.letrec {R.rhs} (fun r -> body (resolve r))
