(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_format.ml,v 1.2 2002/07/19 14:53:45 mauny Exp $ *)

open Pcaml;

EXTEND
  GLOBAL: expr;
  expr: LEVEL "top"
    [ [ n = box_type; d = SELF; "begin";
        el = LIST0 [ e = box_expr; ";" -> e ]; "end" ->
          let el = [<:expr< Format.$lid:"open_" ^ n$ $d$ >> :: el] in
          let el = el @ [<:expr< Format.close_box () >>] in
          <:expr< do { $list:el$ } >>
      | "hbox"; "begin"; el = LIST0 [ e = box_expr; ";" -> e ]; "end" ->
          let el = [<:expr< Format.open_hbox () >> :: el] in
          let el = el @ [<:expr< Format.close_box () >>] in
          <:expr< do { $list:el$ } >>
      | "nobox"; "begin"; el = LIST0 [ e = box_expr; ";" -> e ]; "end" ->
          match el with
          [ [e] -> e
          | _ -> <:expr< do { $list:el$ } >> ] ] ]
  ;
  box_type:
    [ [ n = "hovbox" -> n
      | n = "hvbox" -> n
      | n = "vbox" -> n
      | n = "box" -> n ] ]
  ;
  box_expr:
    [ [ s = STRING -> <:expr< Format.print_string $str:s$ >>
      | UIDENT "STRING"; e = expr -> <:expr< Format.print_string $e$ >>
      | UIDENT "INT"; e = expr -> <:expr< Format.print_int $e$ >>
      | "/-" -> <:expr< Format.print_space () >>
      | "//" -> <:expr< Format.print_cut () >>
      | "!/" -> <:expr< Format.force_newline () >>
      | "?/" -> <:expr< Format.print_if_newline () >>
      | e = expr -> e ] ]
  ;
END;
