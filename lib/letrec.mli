(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


(** Simple interface for generating mutually-recursive functions, for
    the case where recursion is monomorphic, all the functions have
    the same type, and indexes support polymorphic equality.  *)
val letrec : (('a -> 'b code) -> 'a -> 'b code) ->
             (('a -> 'b code) -> 'c code) -> 'c code


(** More elaborate interface for generating mutually-recursive
    functions.  Supports polymorphic recursion, mutually-recursive
    functions of different types, and non-standard definitions of
    equality. *)

(** The equality GADT *)
type (_,_) eql = Refl : ('a,'a) eql


(** The SYMBOL interface to parameterized types with equality *)
module type SYMBOL = sig
  type _ t
  val eql : 'a t -> 'b t -> ('a, 'b) eql option
end

(** The general let-rec interface *)
module type S = sig
  type 'a sym
  type resolve = { resolve: 'a.'a sym -> 'a code }
  type rhs = { rhs: 'a.resolve -> 'a sym -> 'a code }

  val letrec : rhs -> (resolve -> 'b code) -> 'b code
end

(** Build an instance of S from an instance of SYMBOL *)
module Make(Sym: SYMBOL) : S with type 'a sym = 'a Sym.t
