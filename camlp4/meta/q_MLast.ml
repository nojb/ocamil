(* camlp4r pa_extend.cmo pa_extend_m.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: q_MLast.ml,v 1.47 2002/07/19 14:53:50 mauny Exp $ *)

value gram = Grammar.gcreate (Plexer.gmake ());

module Qast =
  struct
    type t =
      [ Node of string and list t
      | List of list t
      | Tuple of list t
      | Option of option t
      | Int of string
      | Str of string
      | Bool of bool
      | Cons of t and t
      | Apply of string and list t
      | Record of list (string * t)
      | Loc
      | Antiquot of MLast.loc and string ]
    ;
    value loc = (0, 0);
    value rec to_expr =
      fun
      [ Node n al ->
          List.fold_left (fun e a -> <:expr< $e$ $to_expr a$ >>)
            <:expr< MLast.$uid:n$ >> al
      | List al ->
          List.fold_right (fun a e -> <:expr< [$to_expr a$ :: $e$] >>) al
            <:expr< [] >>
      | Tuple al -> <:expr< ($list:List.map to_expr al$) >>
      | Option None -> <:expr< None >>
      | Option (Some a) -> <:expr< Some $to_expr a$ >>
      | Int s -> <:expr< $int:s$ >>
      | Str s -> <:expr< $str:s$ >>
      | Bool True -> <:expr< True >>
      | Bool False -> <:expr< False >>
      | Cons a1 a2 -> <:expr< [$to_expr a1$ :: $to_expr a2$] >>
      | Apply f al ->
          List.fold_left (fun e a -> <:expr< $e$ $to_expr a$ >>)
            <:expr< $lid:f$ >> al
      | Record lal -> <:expr< {$list:List.map to_expr_label lal$} >>
      | Loc -> <:expr< $lid:Stdpp.loc_name.val$ >>
      | Antiquot loc s ->
          let e =
            try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s) with
            [ Stdpp.Exc_located (bp, ep) exc ->
                raise (Stdpp.Exc_located (fst loc + bp, fst loc + ep) exc) ]
          in
          <:expr< $anti:e$ >> ]
    and to_expr_label (l, a) = (<:patt< MLast.$lid:l$ >>, to_expr a);
    value rec to_patt =
      fun
      [ Node n al ->
          List.fold_left (fun e a -> <:patt< $e$ $to_patt a$ >>)
            <:patt< MLast.$uid:n$ >> al
      | List al ->
          List.fold_right (fun a p -> <:patt< [$to_patt a$ :: $p$] >>) al
            <:patt< [] >>
      | Tuple al -> <:patt< ($list:List.map to_patt al$) >>
      | Option None -> <:patt< None >>
      | Option (Some a) -> <:patt< Some $to_patt a$ >>
      | Int s -> <:patt< $int:s$ >>
      | Str s -> <:patt< $str:s$ >>
      | Bool True -> <:patt< True >>
      | Bool False -> <:patt< False >>
      | Cons a1 a2 -> <:patt< [$to_patt a1$ :: $to_patt a2$] >>
      | Apply _ _ -> failwith "bad pattern"
      | Record lal -> <:patt< {$list:List.map to_patt_label lal$} >>
      | Loc -> <:patt< _ >>
      | Antiquot loc s ->
          let p =
            try Grammar.Entry.parse Pcaml.patt_eoi (Stream.of_string s) with
            [ Stdpp.Exc_located (bp, ep) exc ->
                raise (Stdpp.Exc_located (fst loc + bp, fst loc + ep) exc) ]
          in
          <:patt< $anti:p$ >> ]
    and to_patt_label (l, a) = (<:patt< MLast.$lid:l$ >>, to_patt a);
  end
;

value antiquot k (bp, ep) x =
  let shift =
    if k = "" then String.length "$"
    else String.length "$" + String.length k + String.length ":"
  in
  Qast.Antiquot (shift + bp, shift + ep) x
;

value sig_item = Grammar.Entry.create gram "signature item";
value str_item = Grammar.Entry.create gram "structure item";
value ctyp = Grammar.Entry.create gram "type";
value patt = Grammar.Entry.create gram "pattern";
value expr = Grammar.Entry.create gram "expression";

value module_type = Grammar.Entry.create gram "module type";
value module_expr = Grammar.Entry.create gram "module expression";

value class_type = Grammar.Entry.create gram "class type";
value class_expr = Grammar.Entry.create gram "class expr";
value class_sig_item = Grammar.Entry.create gram "class signature item";
value class_str_item = Grammar.Entry.create gram "class structure item";

value ipatt = Grammar.Entry.create gram "ipatt";
value let_binding = Grammar.Entry.create gram "let_binding";

value a_list = Grammar.Entry.create gram "a_list";
value a_opt = Grammar.Entry.create gram "a_opt";
value a_UIDENT = Grammar.Entry.create gram "a_UIDENT";
value a_LIDENT = Grammar.Entry.create gram "a_LIDENT";
value a_INT = Grammar.Entry.create gram "a_INT";
value a_FLOAT = Grammar.Entry.create gram "a_FLOAT";
value a_STRING = Grammar.Entry.create gram "a_STRING";
value a_CHAR = Grammar.Entry.create gram "a_CHAR";
value a_TILDEIDENT = Grammar.Entry.create gram "a_TILDEIDENT";
value a_QUESTIONIDENT = Grammar.Entry.create gram "a_QUESTIONIDENT";

value o2b =
  fun
  [ Qast.Option (Some _) -> Qast.Bool True
  | Qast.Option None -> Qast.Bool False
  | x -> x ]
;

value mksequence _ =
  fun
  [ Qast.List [e] -> e
  | el -> Qast.Node "ExSeq" [Qast.Loc; el] ]
;

value mkmatchcase _ p aso w e =
  let p =
    match aso with
    [ Qast.Option (Some p2) -> Qast.Node "PaAli" [Qast.Loc; p; p2]
    | Qast.Option None -> p
    | _ -> Qast.Node "PaAli" [Qast.Loc; p; aso] ]
  in
  Qast.Tuple [p; w; e]
;

value mkumin _ f arg =
  match arg with
  [ Qast.Node "ExInt" [Qast.Loc; Qast.Str n] when int_of_string n > 0 ->
      let n = "-" ^ n in
      Qast.Node "ExInt" [Qast.Loc; Qast.Str n]
  | Qast.Node "ExFlo" [Qast.Loc; Qast.Str n] when float_of_string n > 0.0 ->
      let n = "-" ^ n in
      Qast.Node "ExFlo" [Qast.Loc; Qast.Str n]
  | _ ->
      match f with
      [ Qast.Str f ->
          let f = "~" ^ f in
          Qast.Node "ExApp"
            [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str f]; arg]
      | _ -> assert False ] ]
;

value mkuminpat _ f is_int s =
  match is_int with
  [ Qast.Bool True -> Qast.Node "PaInt" [Qast.Loc; s]
  | Qast.Bool False -> Qast.Node "PaFlo" [Qast.Loc; s]
  | _ -> assert False ]
;

value mklistexp _ last =
  loop True where rec loop top =
    fun
    [ Qast.List [] ->
        match last with
        [ Qast.Option (Some e) -> e
        | Qast.Option None -> Qast.Node "ExUid" [Qast.Loc; Qast.Str "[]"]
        | a -> a ]
    | Qast.List [e1 :: el] ->
        Qast.Node "ExApp"
          [Qast.Loc;
           Qast.Node "ExApp"
             [Qast.Loc; Qast.Node "ExUid" [Qast.Loc; Qast.Str "::"]; e1];
           loop False (Qast.List el)]
    | a -> a ]
;

value mklistpat _ last =
  loop True where rec loop top =
    fun
    [ Qast.List [] ->
        match last with
        [ Qast.Option (Some p) -> p
        | Qast.Option None -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "[]"]
        | a -> a ]
    | Qast.List [p1 :: pl] ->
        Qast.Node "PaApp"
          [Qast.Loc;
           Qast.Node "PaApp"
             [Qast.Loc; Qast.Node "PaUid" [Qast.Loc; Qast.Str "::"]; p1];
           loop False (Qast.List pl)]
    | a -> a ]
;

value mkexprident loc i j =
  loop (Qast.Node "ExUid" [Qast.Loc; i]) j where rec loop m =
    fun
    [ Qast.Node "ExAcc" [_; x; y] ->
        loop (Qast.Node "ExAcc" [Qast.Loc; m; x]) y
    | e -> Qast.Node "ExAcc" [Qast.Loc; m; e] ]
;

value mkassert _ e =
  let f = Qast.Node "ExStr" [Qast.Loc; Qast.Str ""] in
  let bp = Qast.Node "ExInt" [Qast.Loc; Qast.Str "0"] in
  let ep = Qast.Node "ExInt" [Qast.Loc; Qast.Str "0"] in
  let raiser =
    Qast.Node "ExApp"
      [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "raise"];
       Qast.Node "ExApp"
         [Qast.Loc; Qast.Node "ExUid" [Qast.Loc; Qast.Str "Assert_failure"];
          Qast.Node "ExTup" [Qast.Loc; Qast.List [f; bp; ep]]]]
  in
  match e with
  [ Qast.Node "ExUid" [_; Qast.Str "False"] -> raiser
  | _ ->
      if Pcaml.no_assert.val then Qast.Node "ExUid" [Qast.Loc; Qast.Str "()"]
      else
        Qast.Node "ExIfe"
          [Qast.Loc; e; Qast.Node "ExUid" [Qast.Loc; Qast.Str "()"]; raiser] ]
;

value append_elem el e = Qast.Apply "@" [el; Qast.List [e]];

value not_yet_warned_variant = ref True;
value warn_variant () =
  if not_yet_warned_variant.val then do {
    not_yet_warned_variant.val := False;
    Printf.eprintf "\
*** warning: use of syntax of variants types deprecated since version 3.05\n";
    flush stderr
  }
  else ()
;

value not_yet_warned = ref True;
value warn_sequence () =
  if not_yet_warned.val then do {
    not_yet_warned.val := False;
    Printf.eprintf "\
*** warning: use of syntax of sequences deprecated since version 3.01.1\n";
    flush stderr
  }
  else ()
;

EXTEND
  GLOBAL: sig_item str_item ctyp patt expr module_type module_expr class_type
    class_expr class_sig_item class_str_item let_binding ipatt;
  module_expr:
    [ [ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          Qast.Node "MeFun" [Qast.Loc; i; t; me]
      | "struct"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Qast.Node "MeStr" [Qast.Loc; st] ]
    | [ me1 = SELF; me2 = SELF -> Qast.Node "MeApp" [Qast.Loc; me1; me2] ]
    | [ me1 = SELF; "."; me2 = SELF ->
          Qast.Node "MeAcc" [Qast.Loc; me1; me2] ]
    | "simple"
      [ i = a_UIDENT -> Qast.Node "MeUid" [Qast.Loc; i]
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          Qast.Node "MeTyc" [Qast.Loc; me; mt]
      | "("; me = SELF; ")" -> me ] ]
  ;
  str_item:
    [ "top"
      [ "declare"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Qast.Node "StDcl" [Qast.Loc; st]
      | "exception"; ctl = constructor_declaration; b = rebind_exn ->
          let (_, c, tl) =
            match ctl with
            [ Qast.Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Qast.Node "StExc" [Qast.Loc; c; tl; b]
      | "external"; i = a_LIDENT; ":"; t = ctyp; "="; pd = SLIST1 a_STRING ->
          Qast.Node "StExt" [Qast.Loc; i; t; pd]
      | "include"; me = module_expr -> Qast.Node "StInc" [Qast.Loc; me]
      | "module"; i = a_UIDENT; mb = module_binding ->
          Qast.Node "StMod" [Qast.Loc; i; mb]
      | "module"; "type"; i = a_UIDENT; "="; mt = module_type ->
          Qast.Node "StMty" [Qast.Loc; i; mt]
      | "open"; i = mod_ident -> Qast.Node "StOpn" [Qast.Loc; i]
      | "type"; tdl = SLIST1 type_declaration SEP "and" ->
          Qast.Node "StTyp" [Qast.Loc; tdl]
      | "value"; r = rec_flag; l = SLIST1 let_binding SEP "and" ->
          Qast.Node "StVal" [Qast.Loc; r; l]
      | e = expr -> Qast.Node "StExp" [Qast.Loc; e] ] ]
  ;
  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> Qast.List [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = a_UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          Qast.Node "MeFun" [Qast.Loc; m; mt; mb]
      | ":"; mt = module_type; "="; me = module_expr ->
          Qast.Node "MeTyc" [Qast.Loc; me; mt]
      | "="; me = module_expr -> me ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = a_UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          Qast.Node "MtFun" [Qast.Loc; i; t; mt] ]
    | [ mt = SELF; "with"; wcl = SLIST1 with_constr SEP "and" ->
          Qast.Node "MtWit" [Qast.Loc; mt; wcl] ]
    | [ "sig"; sg = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Qast.Node "MtSig" [Qast.Loc; sg] ]
    | [ m1 = SELF; m2 = SELF -> Qast.Node "MtApp" [Qast.Loc; m1; m2] ]
    | [ m1 = SELF; "."; m2 = SELF -> Qast.Node "MtAcc" [Qast.Loc; m1; m2] ]
    | "simple"
      [ i = a_UIDENT -> Qast.Node "MtUid" [Qast.Loc; i]
      | i = a_LIDENT -> Qast.Node "MtLid" [Qast.Loc; i]
      | "'"; i = ident -> Qast.Node "MtQuo" [Qast.Loc; i]
      | "("; mt = SELF; ")" -> mt ] ]
  ;
  sig_item:
    [ "top"
      [ "declare"; st = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Qast.Node "SgDcl" [Qast.Loc; st]
      | "exception"; ctl = constructor_declaration ->
          let (_, c, tl) =
            match ctl with
            [ Qast.Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Qast.Node "SgExc" [Qast.Loc; c; tl]
      | "external"; i = a_LIDENT; ":"; t = ctyp; "="; pd = SLIST1 a_STRING ->
          Qast.Node "SgExt" [Qast.Loc; i; t; pd]
      | "include"; mt = module_type -> Qast.Node "SgInc" [Qast.Loc; mt]
      | "module"; i = a_UIDENT; mt = module_declaration ->
          Qast.Node "SgMod" [Qast.Loc; i; mt]
      | "module"; "type"; i = a_UIDENT; "="; mt = module_type ->
          Qast.Node "SgMty" [Qast.Loc; i; mt]
      | "open"; i = mod_ident -> Qast.Node "SgOpn" [Qast.Loc; i]
      | "type"; tdl = SLIST1 type_declaration SEP "and" ->
          Qast.Node "SgTyp" [Qast.Loc; tdl]
      | "value"; i = a_LIDENT; ":"; t = ctyp ->
          Qast.Node "SgVal" [Qast.Loc; i; t] ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> mt
      | "("; i = a_UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          Qast.Node "MtFun" [Qast.Loc; i; t; mt] ] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tpl = SLIST0 type_parameter; "="; t = ctyp ->
          Qast.Node "WcTyp" [Qast.Loc; i; tpl; t]
      | "module"; i = mod_ident; "="; me = module_expr ->
          Qast.Node "WcMod" [Qast.Loc; i; me] ] ]
  ;
  expr:
    [ "top" RIGHTA
      [ "let"; r = rec_flag; l = SLIST1 let_binding SEP "and"; "in";
        x = SELF ->
          Qast.Node "ExLet" [Qast.Loc; r; l; x]
      | "let"; "module"; m = a_UIDENT; mb = module_binding; "in"; e = SELF ->
          Qast.Node "ExLmd" [Qast.Loc; m; mb; e]
      | "fun"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Qast.Node "ExFun" [Qast.Loc; l]
      | "fun"; p = ipatt; e = fun_def ->
          Qast.Node "ExFun"
            [Qast.Loc; Qast.List [Qast.Tuple [p; Qast.Option None; e]]]
      | "match"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Qast.Node "ExMat" [Qast.Loc; e; l]
      | "match"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          Qast.Node "ExMat"
            [Qast.Loc; e; Qast.List [Qast.Tuple [p1; Qast.Option None; e1]]]
      | "try"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Qast.Node "ExTry" [Qast.Loc; e; l]
      | "try"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          Qast.Node "ExTry"
            [Qast.Loc; e; Qast.List [Qast.Tuple [p1; Qast.Option None; e1]]]
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          Qast.Node "ExIfe" [Qast.Loc; e1; e2; e3]
      | "do"; "{"; seq = sequence; "}" -> mksequence Qast.Loc seq
      | "for"; i = a_LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; "{"; seq = sequence; "}" ->
          Qast.Node "ExFor" [Qast.Loc; i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; "{"; seq = sequence; "}" ->
          Qast.Node "ExWhi" [Qast.Loc; e; seq] ]
    | "where"
      [ e = SELF; "where"; rf = rec_flag; lb = let_binding ->
          Qast.Node "ExLet" [Qast.Loc; rf; Qast.List [lb]; e] ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy ->
          Qast.Node "ExAss" [Qast.Loc; e1; e2] ]
    | "||" RIGHTA
      [ e1 = SELF; "||"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "||"]; e1];
             e2] ]
    | "&&" RIGHTA
      [ e1 = SELF; "&&"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "&&"]; e1];
             e2] ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "<"]; e1];
             e2]
      | e1 = SELF; ">"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str ">"]; e1];
             e2]
      | e1 = SELF; "<="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "<="]; e1];
             e2]
      | e1 = SELF; ">="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str ">="]; e1];
             e2]
      | e1 = SELF; "="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "="]; e1];
             e2]
      | e1 = SELF; "<>"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "<>"]; e1];
             e2]
      | e1 = SELF; "=="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "=="]; e1];
             e2]
      | e1 = SELF; "!="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "!="]; e1];
             e2] ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "^"]; e1];
             e2]
      | e1 = SELF; "@"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "@"]; e1];
             e2] ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "+"]; e1];
             e2]
      | e1 = SELF; "-"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "-"]; e1];
             e2]
      | e1 = SELF; "+."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "+."]; e1];
             e2]
      | e1 = SELF; "-."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "-."]; e1];
             e2] ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "*"]; e1];
             e2]
      | e1 = SELF; "/"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "/"]; e1];
             e2]
      | e1 = SELF; "*."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "*."]; e1];
             e2]
      | e1 = SELF; "/."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "/."]; e1];
             e2]
      | e1 = SELF; "land"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "land"]; e1];
             e2]
      | e1 = SELF; "lor"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lor"]; e1];
             e2]
      | e1 = SELF; "lxor"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lxor"]; e1];
             e2]
      | e1 = SELF; "mod"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "mod"]; e1];
             e2] ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "**"]; e1];
             e2]
      | e1 = SELF; "asr"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "asr"]; e1];
             e2]
      | e1 = SELF; "lsl"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lsl"]; e1];
             e2]
      | e1 = SELF; "lsr"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lsr"]; e1];
             e2] ]
    | "unary minus" NONA
      [ "-"; e = SELF -> mkumin Qast.Loc (Qast.Str "-") e
      | "-."; e = SELF -> mkumin Qast.Loc (Qast.Str "-.") e ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> Qast.Node "ExApp" [Qast.Loc; e1; e2]
      | "assert"; e = SELF -> mkassert Qast.Loc e
      | "lazy"; e = SELF -> Qast.Node "ExLaz" [Qast.Loc; e] ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" ->
          Qast.Node "ExAre" [Qast.Loc; e1; e2]
      | e1 = SELF; "."; "["; e2 = SELF; "]" ->
          Qast.Node "ExSte" [Qast.Loc; e1; e2]
      | e1 = SELF; "."; e2 = SELF -> Qast.Node "ExAcc" [Qast.Loc; e1; e2] ]
    | "~-" NONA
      [ "~-"; e = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "~-"]; e]
      | "~-."; e = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "~-."]; e] ]
    | "simple"
      [ s = a_INT -> Qast.Node "ExInt" [Qast.Loc; s]
      | s = a_FLOAT -> Qast.Node "ExFlo" [Qast.Loc; s]
      | s = a_STRING -> Qast.Node "ExStr" [Qast.Loc; s]
      | s = a_CHAR -> Qast.Node "ExChr" [Qast.Loc; s]
      | i = expr_ident -> i
      | "["; "]" -> Qast.Node "ExUid" [Qast.Loc; Qast.Str "[]"]
      | "["; el = SLIST1 expr SEP ";"; last = cons_expr_opt; "]" ->
          mklistexp Qast.Loc last el
      | "[|"; el = SLIST0 expr SEP ";"; "|]" ->
          Qast.Node "ExArr" [Qast.Loc; el]
      | "{"; lel = SLIST1 label_expr SEP ";"; "}" ->
          Qast.Node "ExRec" [Qast.Loc; lel; Qast.Option None]
      | "{"; "("; e = SELF; ")"; "with"; lel = SLIST1 label_expr SEP ";";
        "}" ->
          Qast.Node "ExRec" [Qast.Loc; lel; Qast.Option (Some e)]
      | "("; ")" -> Qast.Node "ExUid" [Qast.Loc; Qast.Str "()"]
      | "("; e = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "ExTyc" [Qast.Loc; e; t]
      | "("; e = SELF; ","; el = SLIST1 expr SEP ","; ")" ->
          Qast.Node "ExTup" [Qast.Loc; Qast.Cons e el]
      | "("; e = SELF; ")" -> e ] ]
  ;
  cons_expr_opt:
    [ [ "::"; e = expr -> Qast.Option (Some e)
      | -> Qast.Option None ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
  sequence:
    [ [ "let"; rf = rec_flag; l = SLIST1 let_binding SEP "and";
        [ "in" | ";" ]; el = SELF ->
          Qast.List
            [Qast.Node "ExLet" [Qast.Loc; rf; l; mksequence Qast.Loc el]]
      | e = expr; ";"; el = SELF -> Qast.Cons e el
      | e = expr; ";" -> Qast.List [e]
      | e = expr -> Qast.List [e] ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> Qast.Tuple [p; e] ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF ->
          Qast.Node "ExFun"
            [Qast.Loc; Qast.List [Qast.Tuple [p; Qast.Option None; e]]]
      | "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> Qast.Node "ExTyc" [Qast.Loc; e; t] ] ]
  ;
  match_case:
    [ [ p = patt; aso = as_patt_opt; w = when_expr_opt; "->"; e = expr ->
          mkmatchcase Qast.Loc p aso w e ] ]
  ;
  as_patt_opt:
    [ [ "as"; p = patt -> Qast.Option (Some p)
      | -> Qast.Option None ] ]
  ;
  when_expr_opt:
    [ [ "when"; e = expr -> Qast.Option (Some e)
      | -> Qast.Option None ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; e = fun_binding -> Qast.Tuple [i; e] ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = a_LIDENT -> Qast.Node "ExLid" [Qast.Loc; i]
      | i = a_UIDENT -> Qast.Node "ExUid" [Qast.Loc; i]
      | i = a_UIDENT; "."; j = SELF -> mkexprident Qast.Loc i j ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = ipatt; e = SELF ->
          Qast.Node "ExFun"
            [Qast.Loc; Qast.List [Qast.Tuple [p; Qast.Option None; e]]]
      | "->"; e = expr -> e ] ]
  ;
  patt:
    [ LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> Qast.Node "PaOrp" [Qast.Loc; p1; p2] ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> Qast.Node "PaRng" [Qast.Loc; p1; p2] ]
    | LEFTA
      [ p1 = SELF; p2 = SELF -> Qast.Node "PaApp" [Qast.Loc; p1; p2] ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Qast.Node "PaAcc" [Qast.Loc; p1; p2] ]
    | "simple"
      [ s = a_LIDENT -> Qast.Node "PaLid" [Qast.Loc; s]
      | s = a_UIDENT -> Qast.Node "PaUid" [Qast.Loc; s]
      | s = a_INT -> Qast.Node "PaInt" [Qast.Loc; s]
      | s = a_FLOAT -> Qast.Node "PaFlo" [Qast.Loc; s]
      | s = a_STRING -> Qast.Node "PaStr" [Qast.Loc; s]
      | s = a_CHAR -> Qast.Node "PaChr" [Qast.Loc; s]
      | "-"; s = a_INT -> mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool True) s
      | "-"; s = a_FLOAT ->
          mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool False) s
      | "["; "]" -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "[]"]
      | "["; pl = SLIST1 patt SEP ";"; last = cons_patt_opt; "]" ->
          mklistpat Qast.Loc last pl
      | "[|"; pl = SLIST0 patt SEP ";"; "|]" ->
          Qast.Node "PaArr" [Qast.Loc; pl]
      | "{"; lpl = SLIST1 label_patt SEP ";"; "}" ->
          Qast.Node "PaRec" [Qast.Loc; lpl]
      | "("; ")" -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "PaTyc" [Qast.Loc; p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" ->
          Qast.Node "PaAli" [Qast.Loc; p; p2]
      | "("; p = SELF; ","; pl = SLIST1 patt SEP ","; ")" ->
          Qast.Node "PaTup" [Qast.Loc; Qast.Cons p pl]
      | "_" -> Qast.Node "PaAny" [Qast.Loc] ] ]
  ;
  cons_patt_opt:
    [ [ "::"; p = patt -> Qast.Option (Some p)
      | -> Qast.Option None ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> Qast.Tuple [i; p] ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Qast.Node "PaAcc" [Qast.Loc; p1; p2] ]
    | "simple" RIGHTA
      [ i = a_UIDENT -> Qast.Node "PaUid" [Qast.Loc; i]
      | i = a_LIDENT -> Qast.Node "PaLid" [Qast.Loc; i] ] ]
  ;
  ipatt:
    [ [ "{"; lpl = SLIST1 label_ipatt SEP ";"; "}" ->
          Qast.Node "PaRec" [Qast.Loc; lpl]
      | "("; ")" -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "PaTyc" [Qast.Loc; p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" ->
          Qast.Node "PaAli" [Qast.Loc; p; p2]
      | "("; p = SELF; ","; pl = SLIST1 ipatt SEP ","; ")" ->
          Qast.Node "PaTup" [Qast.Loc; Qast.Cons p pl]
      | s = a_LIDENT -> Qast.Node "PaLid" [Qast.Loc; s]
      | "_" -> Qast.Node "PaAny" [Qast.Loc] ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> Qast.Tuple [i; p] ] ]
  ;
  type_declaration:
    [ [ n = type_patt; tpl = SLIST0 type_parameter; "="; tk = ctyp;
        cl = SLIST0 constrain ->
          Qast.Tuple [n; tpl; tk; cl] ] ]
  ;
  type_patt:
    [ [ n = a_LIDENT -> Qast.Tuple [Qast.Loc; n] ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> Qast.Tuple [t1; t2] ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident ->
          Qast.Tuple [i; Qast.Tuple [Qast.Bool False; Qast.Bool False]]
      | "+"; "'"; i = ident ->
          Qast.Tuple [i; Qast.Tuple [Qast.Bool True; Qast.Bool False]]
      | "-"; "'"; i = ident ->
          Qast.Tuple [i; Qast.Tuple [Qast.Bool False; Qast.Bool True]] ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> Qast.Node "TyMan" [Qast.Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> Qast.Node "TyAli" [Qast.Loc; t1; t2] ]
    | LEFTA
      [ "!"; pl = SLIST1 typevar; "."; t = SELF ->
          Qast.Node "TyPol" [Qast.Loc; pl; t] ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> Qast.Node "TyArr" [Qast.Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> Qast.Node "TyApp" [Qast.Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> Qast.Node "TyAcc" [Qast.Loc; t1; t2] ]
    | "simple"
      [ "'"; i = ident -> Qast.Node "TyQuo" [Qast.Loc; i]
      | "_" -> Qast.Node "TyAny" [Qast.Loc]
      | i = a_LIDENT -> Qast.Node "TyLid" [Qast.Loc; i]
      | i = a_UIDENT -> Qast.Node "TyUid" [Qast.Loc; i]
      | "("; t = SELF; "*"; tl = SLIST1 ctyp SEP "*"; ")" ->
          Qast.Node "TyTup" [Qast.Loc; Qast.Cons t tl]
      | "("; t = SELF; ")" -> t
      | "["; cdl = SLIST0 constructor_declaration SEP "|"; "]" ->
          Qast.Node "TySum" [Qast.Loc; cdl]
      | "{"; ldl = SLIST1 label_declaration SEP ";"; "}" ->
          Qast.Node "TyRec" [Qast.Loc; ldl] ] ]
  ;
  constructor_declaration:
    [ [ ci = a_UIDENT; "of"; cal = SLIST1 ctyp SEP "and" ->
          Qast.Tuple [Qast.Loc; ci; cal]
      | ci = a_UIDENT -> Qast.Tuple [Qast.Loc; ci; Qast.List []] ] ]
  ;
  label_declaration:
    [ [ i = a_LIDENT; ":"; mf = mutable_flag; t = ctyp ->
          Qast.Tuple [Qast.Loc; i; mf; t] ] ]
  ;
  ident:
    [ [ i = a_LIDENT -> i
      | i = a_UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = a_UIDENT -> Qast.List [i]
      | i = a_LIDENT -> Qast.List [i]
      | i = a_UIDENT; "."; j = SELF -> Qast.Cons i j ] ]
  ;
  (* Objects and Classes *)
  str_item:
    [ [ "class"; cd = SLIST1 class_declaration SEP "and" ->
          Qast.Node "StCls" [Qast.Loc; cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Qast.Node "StClt" [Qast.Loc; ctd] ] ]
  ;
  sig_item:
    [ [ "class"; cd = SLIST1 class_description SEP "and" ->
          Qast.Node "SgCls" [Qast.Loc; cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Qast.Node "SgClt" [Qast.Loc; ctd] ] ]
  ;
  class_declaration:
    [ [ vf = virtual_flag; i = a_LIDENT; ctp = class_type_parameters;
        cfb = class_fun_binding ->
          Qast.Record
            [("ciLoc", Qast.Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", i);
             ("ciExp", cfb)] ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          Qast.Node "CeTyc" [Qast.Loc; ce; ct]
      | p = ipatt; cfb = SELF -> Qast.Node "CeFun" [Qast.Loc; p; cfb] ] ]
  ;
  class_type_parameters:
    [ [ -> Qast.Tuple [Qast.Loc; Qast.List []]
      | "["; tpl = SLIST1 type_parameter SEP ","; "]" ->
          Qast.Tuple [Qast.Loc; tpl] ] ]
  ;
  class_fun_def:
    [ [ p = ipatt; ce = SELF -> Qast.Node "CeFun" [Qast.Loc; p; ce]
      | "->"; ce = class_expr -> ce ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; p = ipatt; ce = class_fun_def ->
          Qast.Node "CeFun" [Qast.Loc; p; ce]
      | "let"; rf = rec_flag; lb = SLIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          Qast.Node "CeLet" [Qast.Loc; rf; lb; ce] ]
    | "apply" NONA
      [ ce = SELF; e = expr LEVEL "simple" ->
          Qast.Node "CeApp" [Qast.Loc; ce; e] ]
    | "simple"
      [ ci = class_longident; "["; ctcl = SLIST0 ctyp SEP ","; "]" ->
          Qast.Node "CeCon" [Qast.Loc; ci; ctcl]
      | ci = class_longident -> Qast.Node "CeCon" [Qast.Loc; ci; Qast.List []]
      | "object"; cspo = class_self_patt_opt; cf = class_structure; "end" ->
          Qast.Node "CeStr" [Qast.Loc; cspo; cf]
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          Qast.Node "CeTyc" [Qast.Loc; ce; ct]
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = SLIST0 [ cf = class_str_item; ";" -> cf ] -> cf ] ]
  ;
  class_self_patt_opt:
    [ [ "("; p = patt; ")" -> Qast.Option (Some p)
      | "("; p = patt; ":"; t = ctyp; ")" ->
          Qast.Option (Some (Qast.Node "PaTyc" [Qast.Loc; p; t]))
      | -> Qast.Option None ] ]
  ;
  class_str_item:
    [ [ "declare"; st = SLIST0 [ s = class_str_item; ";" -> s ]; "end" ->
          Qast.Node "CrDcl" [Qast.Loc; st]
      | "inherit"; ce = class_expr; pb = as_lident_opt ->
          Qast.Node "CrInh" [Qast.Loc; ce; pb]
      | "value"; labmfe = cvalue ->
          let (lab, mf, e) =
            match labmfe with
            [ Qast.Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Qast.Node "CrVal" [Qast.Loc; lab; mf; e]
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          Qast.Node "CrVir" [Qast.Loc; l; Qast.Bool True; t]
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          Qast.Node "CrVir" [Qast.Loc; l; Qast.Bool False; t]
      | "method"; "private"; l = label; ":"; t = ctyp; "="; e = expr ->
          Qast.Node "CrMth"
            [Qast.Loc; l; Qast.Bool True; e; Qast.Option (Some t)]
      | "method"; "private"; l = label; fb = fun_binding ->
          Qast.Node "CrMth"
            [Qast.Loc; l; Qast.Bool True; fb; Qast.Option None]
      | "method"; l = label; ":"; t = ctyp; "="; e = expr ->
          Qast.Node "CrMth"
            [Qast.Loc; l; Qast.Bool False; e; Qast.Option (Some t)]
      | "method"; l = label; fb = fun_binding ->
          Qast.Node "CrMth"
            [Qast.Loc; l; Qast.Bool False; fb; Qast.Option None]
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          Qast.Node "CrCtr" [Qast.Loc; t1; t2]
      | "initializer"; se = expr -> Qast.Node "CrIni" [Qast.Loc; se] ] ]
  ;
  as_lident_opt:
    [ [ "as"; i = a_LIDENT -> Qast.Option (Some i)
      | -> Qast.Option None ] ]
  ;
  cvalue:
    [ [ mf = mutable_flag; l = label; "="; e = expr -> Qast.Tuple [l; mf; e]
      | mf = mutable_flag; l = label; ":"; t = ctyp; "="; e = expr ->
          Qast.Tuple [l; mf; Qast.Node "ExTyc" [Qast.Loc; e; t]]
      | mf = mutable_flag; l = label; ":"; t = ctyp; ":>"; t2 = ctyp; "=";
        e = expr ->
          Qast.Tuple
            [l; mf; Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option (Some t); t2]]
      | mf = mutable_flag; l = label; ":>"; t = ctyp; "="; e = expr ->
          Qast.Tuple
            [l; mf; Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option None; t]] ] ]
  ;
  label:
    [ [ i = a_LIDENT -> i ] ]
  ;
  class_type:
    [ [ "["; t = ctyp; "]"; "->"; ct = SELF ->
          Qast.Node "CtFun" [Qast.Loc; t; ct]
      | id = clty_longident; "["; tl = SLIST1 ctyp SEP ","; "]" ->
          Qast.Node "CtCon" [Qast.Loc; id; tl]
      | id = clty_longident -> Qast.Node "CtCon" [Qast.Loc; id; Qast.List []]
      | "object"; cst = SOPT class_self_type;
        csf = SLIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          Qast.Node "CtSig" [Qast.Loc; cst; csf] ] ]
  ;
  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;
  class_sig_item:
    [ [ "declare"; st = SLIST0 [ s = class_sig_item; ";" -> s ]; "end" ->
          Qast.Node "CgDcl" [Qast.Loc; st]
      | "inherit"; cs = class_type -> Qast.Node "CgInh" [Qast.Loc; cs]
      | "value"; mf = mutable_flag; l = label; ":"; t = ctyp ->
          Qast.Node "CgVal" [Qast.Loc; l; mf; t]
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          Qast.Node "CgVir" [Qast.Loc; l; Qast.Bool True; t]
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          Qast.Node "CgVir" [Qast.Loc; l; Qast.Bool False; t]
      | "method"; "private"; l = label; ":"; t = ctyp ->
          Qast.Node "CgMth" [Qast.Loc; l; Qast.Bool True; t]
      | "method"; l = label; ":"; t = ctyp ->
          Qast.Node "CgMth" [Qast.Loc; l; Qast.Bool False; t]
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          Qast.Node "CgCtr" [Qast.Loc; t1; t2] ] ]
  ;
  class_description:
    [ [ vf = virtual_flag; n = a_LIDENT; ctp = class_type_parameters; ":";
        ct = class_type ->
          Qast.Record
            [("ciLoc", Qast.Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", n);
             ("ciExp", ct)] ] ]
  ;
  class_type_declaration:
    [ [ vf = virtual_flag; n = a_LIDENT; ctp = class_type_parameters; "=";
        cs = class_type ->
          Qast.Record
            [("ciLoc", Qast.Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", n);
             ("ciExp", cs)] ] ]
  ;
  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> Qast.Node "ExNew" [Qast.Loc; i] ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> Qast.Node "ExSnd" [Qast.Loc; e; lab] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
          Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option (Some t); t2]
      | "("; e = SELF; ":>"; t = ctyp; ")" ->
          Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option None; t]
      | "{<"; ">}" -> Qast.Node "ExOvr" [Qast.Loc; Qast.List []]
      | "{<"; fel = field_expr_list; ">}" ->
          Qast.Node "ExOvr" [Qast.Loc; fel] ] ]
  ;
  field_expr_list:
    [ [ l = label; "="; e = expr; ";"; fel = SELF ->
          Qast.Cons (Qast.Tuple [l; e]) fel
      | l = label; "="; e = expr; ";" -> Qast.List [Qast.Tuple [l; e]]
      | l = label; "="; e = expr -> Qast.List [Qast.Tuple [l; e]] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> Qast.Node "TyCls" [Qast.Loc; id]
      | "<"; mlv = meth_list; ">" ->
          let (ml, v) =
            match mlv with
            [ Qast.Tuple [xx1; xx2] -> (xx1, xx2)
            | _ -> match () with [] ]
          in
          Qast.Node "TyObj" [Qast.Loc; ml; v]
      | "<"; ">" ->
          Qast.Node "TyObj" [Qast.Loc; Qast.List []; Qast.Bool False] ] ]
  ;
  meth_list:
    [ [ f = field; ";"; mlv = SELF ->
          let (ml, v) =
            match mlv with
            [ Qast.Tuple [xx1; xx2] -> (xx1, xx2)
            | _ -> match () with [] ]
          in
          Qast.Tuple [Qast.Cons f ml; v]
      | f = field; ";" -> Qast.Tuple [Qast.List [f]; Qast.Bool False]
      | f = field -> Qast.Tuple [Qast.List [f]; Qast.Bool False]
      | ".." -> Qast.Tuple [Qast.List []; Qast.Bool True] ] ]
  ;
  field:
    [ [ lab = a_LIDENT; ":"; t = ctyp -> Qast.Tuple [lab; t] ] ]
  ;
  typevar:
    [ [ "'"; i = ident -> i ] ]
  ;
  clty_longident:
    [ [ m = a_UIDENT; "."; l = SELF -> Qast.Cons m l
      | i = a_LIDENT -> Qast.List [i] ] ]
  ;
  class_longident:
    [ [ m = a_UIDENT; "."; l = SELF -> Qast.Cons m l
      | i = a_LIDENT -> Qast.List [i] ] ]
  ;
  (* Labels *)
  ctyp: AFTER "arrow"
    [ NONA
      [ i = a_TILDEIDENT; ":"; t = SELF -> Qast.Node "TyLab" [Qast.Loc; i; t]
      | i = a_QUESTIONIDENT; ":"; t = SELF ->
          Qast.Node "TyOlb" [Qast.Loc; i; t] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "["; "="; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn" [Qast.Loc; rfl; Qast.Option None]
      | "["; ">"; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option None))]
      | "["; "<"; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl;
             Qast.Option (Some (Qast.Option (Some (Qast.List []))))]
      | "["; "<"; rfl = row_field_list; ">"; ntl = SLIST1 name_tag; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option (Some ntl)))] ] ]
  ;
  row_field_list:
    [ [ rfl = SLIST0 row_field SEP "|" -> rfl ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> Qast.Node "RfTag" [i; Qast.Bool True; Qast.List []]
      | "`"; i = ident; "of"; ao = SOPT "&"; l = SLIST1 ctyp SEP "&" ->
          Qast.Node "RfTag" [i; o2b ao; l]
      | t = ctyp -> Qast.Node "RfInh" [t] ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  patt: LEVEL "simple"
    [ [ "`"; s = ident -> Qast.Node "PaVrn" [Qast.Loc; s]
      | "#"; sl = mod_ident -> Qast.Node "PaTyp" [Qast.Loc; sl]
      | i = a_TILDEIDENT; ":"; p = SELF -> Qast.Node "PaLab" [Qast.Loc; i; p]
      | i = a_TILDEIDENT ->
          Qast.Node "PaLab" [Qast.Loc; i; Qast.Node "PaLid" [Qast.Loc; i]]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ")" ->
          Qast.Node "PaOlb" [Qast.Loc; i; p; Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; "="; e = expr; ")" ->
          Qast.Node "PaOlb" [Qast.Loc; i; p; Qast.Option (Some e)]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaTyc" [Qast.Loc; p; t];
             Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; "="; e = expr;
        ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaTyc" [Qast.Loc; p; t];
             Qast.Option (Some e)]
      | i = a_QUESTIONIDENT ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaLid" [Qast.Loc; i]; Qast.Option None]
      | "?"; "("; i = a_LIDENT; "="; e = expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaLid" [Qast.Loc; i];
             Qast.Option (Some e)]
      | "?"; "("; i = a_LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i;
             Qast.Node "PaTyc" [Qast.Loc; Qast.Node "PaLid" [Qast.Loc; i]; t];
             Qast.Option (Some e)] ] ]
  ;
  ipatt:
    [ [ i = a_TILDEIDENT; ":"; p = SELF -> Qast.Node "PaLab" [Qast.Loc; i; p]
      | i = a_TILDEIDENT ->
          Qast.Node "PaLab" [Qast.Loc; i; Qast.Node "PaLid" [Qast.Loc; i]]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ")" ->
          Qast.Node "PaOlb" [Qast.Loc; i; p; Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; "="; e = expr; ")" ->
          Qast.Node "PaOlb" [Qast.Loc; i; p; Qast.Option (Some e)]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaTyc" [Qast.Loc; p; t];
             Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; "="; e = expr;
        ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaTyc" [Qast.Loc; p; t];
             Qast.Option (Some e)]
      | i = a_QUESTIONIDENT ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaLid" [Qast.Loc; i]; Qast.Option None]
      | "?"; "("; i = a_LIDENT; "="; e = expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Node "PaLid" [Qast.Loc; i];
             Qast.Option (Some e)]
      | "?"; "("; i = a_LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i;
             Qast.Node "PaTyc" [Qast.Loc; Qast.Node "PaLid" [Qast.Loc; i]; t];
             Qast.Option (Some e)] ] ]
  ;
  expr: AFTER "apply"
    [ "label" NONA
      [ i = a_TILDEIDENT; ":"; e = SELF -> Qast.Node "ExLab" [Qast.Loc; i; e]
      | i = a_TILDEIDENT ->
          Qast.Node "ExLab" [Qast.Loc; i; Qast.Node "ExLid" [Qast.Loc; i]]
      | i = a_QUESTIONIDENT; ":"; e = SELF ->
          Qast.Node "ExOlb" [Qast.Loc; i; e]
      | i = a_QUESTIONIDENT ->
          Qast.Node "ExOlb" [Qast.Loc; i; Qast.Node "ExLid" [Qast.Loc; i]] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> Qast.Node "ExVrn" [Qast.Loc; s] ] ]
  ;
  rec_flag:
    [ [ "rec" -> Qast.Bool True
      | -> Qast.Bool False ] ]
  ;
  direction_flag:
    [ [ "to" -> Qast.Bool True
      | "downto" -> Qast.Bool False ] ]
  ;
  mutable_flag:
    [ [ "mutable" -> Qast.Bool True
      | -> Qast.Bool False ] ]
  ;
  virtual_flag:
    [ [ "virtual" -> Qast.Bool True
      | -> Qast.Bool False ] ]
  ;
  (* Compatibility old syntax of variant types definitions *)
  ctyp: LEVEL "simple"
    [ [ "[|"; warning_variant; rfl = row_field_list; "|]" ->
          Qast.Node "TyVrn" [Qast.Loc; rfl; Qast.Option None]
      | "[|"; warning_variant; ">"; rfl = row_field_list; "|]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option None))]
      | "[|"; warning_variant; "<"; rfl = row_field_list; "|]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl;
             Qast.Option (Some (Qast.Option (Some (Qast.List []))))]
      | "[|"; warning_variant; "<"; rfl = row_field_list; ">";
        ntl = SLIST1 name_tag; "|]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option (Some ntl)))] ] ]
  ;
  warning_variant:
    [ [ -> warn_variant () ] ]
  ;
  (* Compatibility old syntax of sequences *)
  expr: LEVEL "top"
    [ [ "do"; seq = SLIST0 [ e = expr; ";" -> e ]; "return"; warning_sequence;
        e = SELF ->
          Qast.Node "ExSeq" [Qast.Loc; append_elem seq e]
      | "for"; i = a_LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = SLIST0 [ e = expr; ";" -> e ]; warning_sequence; "done" ->
          Qast.Node "ExFor" [Qast.Loc; i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; seq = SLIST0 [ e = expr; ";" -> e ];
        warning_sequence; "done" ->
          Qast.Node "ExWhi" [Qast.Loc; e; seq] ] ]
  ;
  warning_sequence:
    [ [ -> warn_sequence () ] ]
  ;
  (* Antiquotations for local entries *)
  sequence:
    [ [ a = ANTIQUOT "list" -> antiquot "list" loc a ] ]
  ;
  expr_ident:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  patt_label_ident: LEVEL "simple"
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  when_expr_opt:
    [ [ a = ANTIQUOT "when" -> antiquot "when" loc a ] ]
  ;
  mod_ident:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  class_self_patt_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a
      | a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  as_lident_opt:
    [ [ a = ANTIQUOT "as" -> antiquot "as" loc a ] ]
  ;
  meth_list:
    [ [ a = a_list -> Qast.Tuple [a; Qast.Bool False]
      | a = a_list; b = ANTIQUOT -> Qast.Tuple [a; antiquot "" loc b] ] ]
  ;
  clty_longident:
    [ [ a = a_list -> a ] ]
  ;
  class_longident:
    [ [ a = a_list -> a ] ]
  ;
  rec_flag:
    [ [ a = ANTIQUOT "rec" -> antiquot "rec" loc a ] ]
  ;
  direction_flag:
    [ [ a = ANTIQUOT "to" -> antiquot "to" loc a ] ]
  ;
  mutable_flag:
    [ [ a = ANTIQUOT "mut" -> antiquot "mut" loc a ] ]
  ;
  virtual_flag:
    [ [ a = ANTIQUOT "virt" -> antiquot "virt" loc a ] ]
  ;
  (* compatibility hack with version 3.04 *)
  class_expr: LEVEL "simple"
    [ [ "object"; cspo = ANTIQUOT; cf = class_structure; "end" ->
          Qast.Node "CeStr" [Qast.Loc; antiquot "" loc cspo; cf] ] ]
  ;
END;

EXTEND
  GLOBAL: str_item sig_item;
  str_item:
    [ [ "#"; n = a_LIDENT; dp = dir_param ->
          Qast.Node "StDir" [Qast.Loc; n; dp] ] ]
  ;
  sig_item:
    [ [ "#"; n = a_LIDENT; dp = dir_param ->
          Qast.Node "SgDir" [Qast.Loc; n; dp] ] ]
  ;
  dir_param:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a
      | e = expr -> Qast.Option (Some e)
      | -> Qast.Option None ] ]
  ;
END;

(* Antiquotations *)

EXTEND
  module_expr: LEVEL "simple"
    [ [ a = ANTIQUOT "mexp" -> antiquot "mexp" loc a
      | a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  str_item: LEVEL "top"
    [ [ a = ANTIQUOT "stri" -> antiquot "stri" loc a
      | a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  module_type: LEVEL "simple"
    [ [ a = ANTIQUOT "mtyp" -> antiquot "mtyp" loc a
      | a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  sig_item: LEVEL "top"
    [ [ a = ANTIQUOT "sigi" -> antiquot "sigi" loc a
      | a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  expr: LEVEL "simple"
    [ [ a = ANTIQUOT "exp" -> antiquot "exp" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | a = ANTIQUOT "anti" ->
          Qast.Node "ExAnt" [Qast.Loc; antiquot "anti" loc a]
      | "("; el = a_list; ")" -> Qast.Node "ExTup" [Qast.Loc; el] ] ]
  ;
  patt: LEVEL "simple"
    [ [ a = ANTIQUOT "pat" -> antiquot "pat" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | a = ANTIQUOT "anti" ->
          Qast.Node "PaAnt" [Qast.Loc; antiquot "anti" loc a]
      | "("; pl = a_list; ")" -> Qast.Node "PaTup" [Qast.Loc; pl] ] ]
  ;
  ipatt:
    [ [ a = ANTIQUOT "pat" -> antiquot "pat" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | a = ANTIQUOT "anti" ->
          Qast.Node "PaAnt" [Qast.Loc; antiquot "anti" loc a]
      | "("; pl = a_list; ")" -> Qast.Node "PaTup" [Qast.Loc; pl] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ a = ANTIQUOT "typ" -> antiquot "typ" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | "("; tl = a_list; ")" -> Qast.Node "TyTup" [Qast.Loc; tl] ] ]
  ;
  class_expr: LEVEL "simple"
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  class_str_item:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  class_sig_item:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  class_type:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  expr: LEVEL "simple"
    [ [ "{<"; fel = a_list; ">}" -> Qast.Node "ExOvr" [Qast.Loc; fel] ] ]
  ;
  patt: LEVEL "simple"
    [ [ "#"; a = a_list -> Qast.Node "PaTyp" [Qast.Loc; a] ] ]
  ;
  a_list:
    [ [ a = ANTIQUOT "list" -> antiquot "list" loc a ] ]
  ;
  a_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a ] ]
  ;
  a_UIDENT:
    [ [ a = ANTIQUOT "uid" -> antiquot "uid" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | i = UIDENT -> Qast.Str i ] ]
  ;
  a_LIDENT:
    [ [ a = ANTIQUOT "lid" -> antiquot "lid" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | i = LIDENT -> Qast.Str i ] ]
  ;
  a_INT:
    [ [ a = ANTIQUOT "int" -> antiquot "int" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | s = INT -> Qast.Str s ] ]
  ;
  a_FLOAT:
    [ [ a = ANTIQUOT "flo" -> antiquot "flo" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | s = FLOAT -> Qast.Str s ] ]
  ;
  a_STRING:
    [ [ a = ANTIQUOT "str" -> antiquot "str" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | s = STRING -> Qast.Str s ] ]
  ;
  a_CHAR:
    [ [ a = ANTIQUOT "chr" -> antiquot "chr" loc a
      | a = ANTIQUOT -> antiquot "" loc a
      | s = CHAR -> Qast.Str s ] ]
  ;
  a_TILDEIDENT:
    [ [ "~"; a = ANTIQUOT -> antiquot "" loc a
      | s = TILDEIDENT -> Qast.Str s ] ]
  ;
  a_QUESTIONIDENT:
    [ [ "?"; a = ANTIQUOT -> antiquot "" loc a
      | s = QUESTIONIDENT -> Qast.Str s ] ]
  ;
END;

value apply_entry e =
  let f s = Grammar.Entry.parse e (Stream.of_string s) in
  let expr s = Qast.to_expr (f s) in
  let patt s = Qast.to_patt (f s) in
  Quotation.ExAst (expr, patt)
;

let sig_item_eoi = Grammar.Entry.create gram "signature item" in
do {
  EXTEND
    sig_item_eoi:
      [ [ x = sig_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "sig_item" (apply_entry sig_item_eoi)
};

let str_item_eoi = Grammar.Entry.create gram "structure item" in
do {
  EXTEND
    str_item_eoi:
      [ [ x = str_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "str_item" (apply_entry str_item_eoi)
};

let ctyp_eoi = Grammar.Entry.create gram "type" in
do {
  EXTEND
    ctyp_eoi:
      [ [ x = ctyp; EOI -> x ] ]
    ;
  END;
  Quotation.add "ctyp" (apply_entry ctyp_eoi)
};

let patt_eoi = Grammar.Entry.create gram "pattern" in
do {
  EXTEND
    patt_eoi:
      [ [ x = patt; EOI -> x ] ]
    ;
  END;
  Quotation.add "patt" (apply_entry patt_eoi)
};

let expr_eoi = Grammar.Entry.create gram "expression" in
do {
  EXTEND
    expr_eoi:
      [ [ x = expr; EOI -> x ] ]
    ;
  END;
  Quotation.add "expr" (apply_entry expr_eoi)
};

let module_type_eoi = Grammar.Entry.create gram "module type" in
do {
  EXTEND
    module_type_eoi:
      [ [ x = module_type; EOI -> x ] ]
    ;
  END;
  Quotation.add "module_type" (apply_entry module_type_eoi)
};

let module_expr_eoi = Grammar.Entry.create gram "module expression" in
do {
  EXTEND
    module_expr_eoi:
      [ [ x = module_expr; EOI -> x ] ]
    ;
  END;
  Quotation.add "module_expr" (apply_entry module_expr_eoi)
};

let class_type_eoi = Grammar.Entry.create gram "class_type" in
do {
  EXTEND
    class_type_eoi:
      [ [ x = class_type; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_type" (apply_entry class_type_eoi)
};

let class_expr_eoi = Grammar.Entry.create gram "class_expr" in
do {
  EXTEND
    class_expr_eoi:
      [ [ x = class_expr; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_expr" (apply_entry class_expr_eoi)
};

let class_sig_item_eoi = Grammar.Entry.create gram "class_sig_item" in
do {
  EXTEND
    class_sig_item_eoi:
      [ [ x = class_sig_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_sig_item" (apply_entry class_sig_item_eoi)
};

let class_str_item_eoi = Grammar.Entry.create gram "class_str_item" in
do {
  EXTEND
    class_str_item_eoi:
      [ [ x = class_str_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_str_item" (apply_entry class_str_item_eoi)
};
