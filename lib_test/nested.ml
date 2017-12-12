(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Generating nested let-rec bindings *)
let mul =
  let%staged rec mul () =
    let add = 
      let%staged rec add () =
        .< fun x y -> if x = 0 then y else (.~(mul ()) 1 1) + (.~(add ()) (x - 1) y) >.
      in add ()
    in .< fun x y -> if x = 1 then y else .~add y (.~(mul ()) (x - 1) y) >.
  in mul ()
