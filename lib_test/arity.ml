(*
 * Copyright (c) 2025 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Letrec

module Sym = struct
  type _ t =
    Z : int t
  | S : 'a t -> (int -> 'a) t
  let rec eql : type a b. a t -> b t -> (a, b) eql option =
    fun x y ->
    match x, y with
    | Z , Z -> Some Refl
    | S x, S y -> begin match eql x y with
                  | Some Refl -> Some Refl
                  | None -> None
                  end
    | _ -> None
end

module R = Letrec.Make(Sym)

let rec mkrhs : type a. (int code list -> int code) -> a Sym.t -> a code =
  fun k -> function
  | Z -> k []
  | S n -> .< fun x -> .~(mkrhs (fun l -> k (.<x>. :: l)) n)>.
         
let sumn n =
  let rhs _ = mkrhs (List.fold_left (fun x y -> .< .~x + .~y >.) .<0>.) in
  R.letrec {R.rhs} (fun {R.resolve} -> resolve n)
