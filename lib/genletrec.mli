(*
 * Copyright (c) 2018 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Low-level interface for let-rec generation *)

type locus_t
val genletrec_locus: (locus_t -> 'a code) -> 'a code
val genletrec : locus_t -> ('a code -> 'a code) -> 'a code 
