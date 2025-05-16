(*
 Lightweight syntactic sugar for the staged 'let rec'.

   let rec%staged f = e in e'
   ~>
     Letrec.letrec (fun f -> e) (fun f -> e')

   let rec%staged[@eq p] f = e in e'
   ~>
     Letrec.letrec ~equal:p (fun f -> e) (fun f -> e')

*)

(* TODO: support for the fancier polymorphic letrec *)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

let rec filter_map f = function
  | [] -> []
  | x :: xs -> match f x with None -> filter_map f xs | Some y -> y :: filter_map f xs

(* TODO: more careful handling of locations *)
let map_let mapper = function
  | { pexp_desc = Pexp_extension ({txt = "staged"; loc}, str) ; pexp_attributes } ->
    let str = default_mapper.payload mapper str in
    begin match str with
      | PStr [ {pstr_desc = 
                  Pstr_eval ({pexp_desc = Pexp_let (Recursive, vbs, body)} as e,_)} ] ->
        begin match vbs with
          | [{ pvb_pat=({ppat_desc=Ppat_var _} as x);
               pvb_expr = rhs; pvb_attributes }] ->
             let eq = match Attr_support.find_attr "eq" pvb_attributes with
               | None -> []
               | Some (PStr [{pstr_desc = Pstr_eval (eq,_)}]) -> [Labelled "equal", eq]
               | Some _ -> failwith "the attribute [@eq e] is of the wrong form"
             in
             let params = [{ pparam_loc = Location.none;
                             pparam_desc = Pparam_val (Nolabel, None, x) } ] in
             let id = Location.mknoloc
                        (Option.get (Longident.unflatten ["Letrec";"letrec"])) in
             {e with pexp_attributes ; pexp_desc =
                      Pexp_apply ({e with pexp_desc = Pexp_ident id},
                                  eq @
                                  [(Nolabel, {rhs with pexp_desc =
                                                         Pexp_function (params, None, Pfunction_body rhs)});
                                   (Nolabel, {rhs with pexp_desc =
                                                         Pexp_function (params, None, Pfunction_body body)})])}
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
