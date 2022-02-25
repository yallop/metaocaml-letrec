(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* An artificial example with an index type that doesn't support
   OCaml's built-in equality (=) *)

(* functional representation of 
   type eo = Even | Odd *)
type eo = { eo: 'a. 'a -> 'a -> 'a }
let eo_eq {eo=l} {eo=r} = l 0 1 = r 0 1
let even = { eo = fun e _ -> e }
and odd  = { eo = fun _ o -> o }

let evenp_oddp =
  let%staged[@eq eo_eq] rec eo = fun {eo=i} ->
    i (.< fun x -> x =  0 || .~(eo odd)  (x - 1) >.)
      (.< fun x -> x <> 0 && .~(eo even) (x - 1) >.)
  in .< (.~(eo even), .~(eo odd)) >.
