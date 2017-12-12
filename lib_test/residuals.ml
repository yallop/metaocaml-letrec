(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Generalization of even/odd to residuals mod n *)
let residuals n =
  let%staged rec res i =
    if i = 0 then .< fun x -> x  = 0 || .~(res (n - 1)) (x - 1) >.
    else          .< fun x -> x <> 0 && .~(res (i - 1)) (x - 1) >.
  in res (n - 1)
