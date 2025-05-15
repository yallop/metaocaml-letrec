open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

dispatch begin
  function
  | After_rules ->
     begin match Sys.ocaml_version with
     | "5.3.0" ->
        copy_rule "attr_support" "lib/attr_support_530.ml" "lib/attr_support.ml";
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Codelib"];
     | version ->
        Printf.ksprintf failwith "Unsupported OCaml version %s" version
     end
  | _ -> ()
end;;

