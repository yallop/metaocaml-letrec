(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Letrec

type 'a ntree =
    EmptyN
  | TreeN of 'a * ('a * 'a) ntree

type swiv = { swiv: 'a. ('a -> 'a) -> 'a ntree -> 'a ntree } [@@unboxed]

module Sym_swiv = struct
  type _ t = Swiv : swiv t
  let eql : type a b. a t -> b t -> (a, b) eql option =
    fun Swiv Swiv -> Some Refl
end
