(* Code is defined in 4.04.0+BER as
     type code_repr = 
       Code of flvars * Parsetree.expression

   and in 4.03.0+effects-ber as

     type code_repr =
         Code of string loc heap * Parsetree.expression

   I think in both cases we can safely ignore the first component.

   Question in both cases: what happens if we genlet an expression
   that refers to a rec-bound variable?  I *think* that the genlet
   returns the variable immediately when executed.
*)

let add_attr (type a) s (c : a code) : a code =
  let h, e = Obj.magic c in
  Obj.magic (h, Ast_helper.Exp.attr e
                  (Location.mknoloc s, Parsetree.PStr []))

let has_attr name attrs =
  List.exists (fun ({Asttypes.txt},_) -> txt = name) attrs

let find_attr name attrs =
  match List.find (fun ({Asttypes.txt}, _) -> txt = name) attrs with
  | (_,p) -> Some p
  | exception Not_found -> None

let remove_attr name attrs =
  snd (List.partition (fun ({Asttypes.txt},_) -> txt = name) attrs)

let mkletrec binds e =
  let open Parsetree in
  { pexp_desc = Pexp_let (Recursive, binds, e);
    pexp_loc = Location.none;
    pexp_attributes = [] }
