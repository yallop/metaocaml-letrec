(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let () = begin
  let pr fmt = Format.fprintf Format.std_formatter fmt in
  
  pr "(* Ackermann function (4) *)@\n";
  pr "%a@." Print_code.print_code (Ackermann.ack 4);

  pr "(* Even/odd *)@\n";
  pr "%a@." Print_code.print_code Even_odd.evenp_oddp;

  pr "(* Residuals modulo n *)@\n";
  pr "%a@." Print_code.print_code (Residuals.residuals 4);

  pr "(* Polymorphic recursion over nested types *)@\n";
  pr "%a@." Print_code.print_code Polymorphic.swiv;

  pr "(* Nested let-rec generation *)@\n";
  pr "%a@." Print_code.print_code Nested.mul;

  pr "(* Mutual recursion with non-functions *)@\n";
  pr "%a@." Print_code.print_code Recursive_values.(values 4);
end
