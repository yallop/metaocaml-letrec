(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Test_types
open Sym_swiv

(* Polymorphic recursion over nested data types *)

module R = Letrec.Make(Sym_swiv)

let swiv =
  let rhs : type a. R.resolve -> a R.sym -> a code =
     fun {R.resolve=swiv} Swiv ->
       .< { swiv = fun f -> function
           | EmptyN ->
             EmptyN
           | TreeN (v, t) ->
             TreeN (f v, (.~(swiv Swiv)).swiv (fun (x, y) -> (f y, f x)) t)} >.
  in
  R.letrec {R.rhs} (fun {R.resolve=swiv} -> swiv Swiv)
