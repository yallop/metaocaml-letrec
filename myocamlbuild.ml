open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

dispatch begin
  function
  | After_rules ->
     begin match Sys.ocaml_version with
     | "4.04.0" -> 
        copy_rule "attr_support" "lib/attr_support_407.ml" "lib/attr_support.ml";
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Print_code"]
     | "4.07.1"  ->
        copy_rule "attr_support" "lib/attr_support_407.ml" "lib/attr_support.ml";
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Codelib"]
     | "4.11.1" ->
        copy_rule "attr_support" "lib/attr_support_411.ml" "lib/attr_support.ml";
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Codelib"];
     | "4.14.1" ->
        copy_rule "attr_support" "lib/attr_support_414.ml" "lib/attr_support.ml";
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Codelib"];
     | version ->
        Printf.kprintf failwith "Unsupported OCaml version %s" version
     end
  | _ -> ()
end;;

