(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Generating mutually-recursive values *)
let values n =
  let%staged rec vals i =
    if i = n then .< n :: .~(vals 0) >.
    else .< i :: .~(vals (succ i)) >.
  in vals 0
