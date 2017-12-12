(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let ack x = 
  let%staged rec ack m =
    .< fun n -> .~(if m = 0 then .<n+1>.
                   else .< if n = 0 then .~(ack (m - 1)) 1
                           else .~(ack (m - 1)) (.~(ack m) (n - 1)) >.)>.
  in ack x
