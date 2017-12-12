(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type eo = Even | Odd

let evenp_oddp =
  let%staged rec eo = function
     | Even -> .< fun x -> x =  0 || .~(eo Odd)  (x - 1) >.
     | Odd ->  .< fun x -> x <> 0 && .~(eo Even) (x - 1) >.
  in .< (.~(eo Even), .~(eo Odd)) >.
