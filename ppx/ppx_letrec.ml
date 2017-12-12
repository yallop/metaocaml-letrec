(*
 Lightweight syntactic sugar for the staged 'let rec'.

   let rec%staged f = e in e'
   ~>
     Letrec.letrec (fun f -> e) (fun f -> e')
*)

(* TODO: support for the fancier polymorphic letrec *)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

(* TODO: more careful handling of locations *)
let map_let mapper = function
  | { pexp_desc = Pexp_extension ({txt = "staged"; loc}, str) } ->
    let str = default_mapper.payload mapper str in
    begin match str with
      | PStr [ {pstr_desc = 
                  Pstr_eval ({pexp_desc = Pexp_let (Recursive, vbs, body)} as e,_)} ] ->
        begin match vbs with
          | [{ pvb_pat=({ppat_desc=Ppat_var _} as x);
               pvb_expr = rhs }] ->
            {e with pexp_desc =
                      Pexp_apply ({e with pexp_desc = Pexp_ident (Location.mknoloc (Longident.parse "Letrec.letrec"))},
                                  [(Nolabel, {rhs with pexp_desc =
                                                         Pexp_fun (Nolabel, None, x, rhs)});
                                   (Nolabel, {rhs with pexp_desc =
                                                         Pexp_fun (Nolabel, None, x, body)})])}
          | [_] -> 
            failwith "let%staged requires a binding of the form: let%staged rec f = ..."
          | _ ->
            failwith "let%staged must be given exactly one binding" 
        end
      | str -> 
        failwith "let%staged requires a let binding"
              
    end
  | e -> default_mapper.expr mapper e
    
let () = register "staged" (fun _ -> { default_mapper with expr = map_let })
