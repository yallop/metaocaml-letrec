let add_attr (type a) s (c : a code) : a code =
  let h, e = Obj.magic c in
  Obj.magic (h, Ast_helper.Exp.attr e {attr_name = Location.mknoloc s;  attr_loc = Location.none;
                                       attr_payload = Parsetree.PStr [];})


let find_attr name attrs =
  match List.find (fun {Parsetree.attr_name = {Asttypes.txt}} -> txt = name) attrs with
  | { Parsetree.attr_payload = p } -> Some p
  | exception Not_found -> None

let has_attr name attrs =
  List.exists (fun {Parsetree.attr_name} -> attr_name.txt = name) attrs

let remove_attr name attrs =
  snd (List.partition (fun {Parsetree.attr_name} -> attr_name.txt = name) attrs)

let mkletrec binds e =
  let open Parsetree in
  { pexp_desc = Pexp_let (Recursive, binds, e);
    pexp_loc = Location.none;
    pexp_loc_stack = [];
    pexp_attributes = [] }
