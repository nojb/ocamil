(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: printulambda.ml,v 1.28 2006/10/16 12:45:55 montela Exp $ *)

open Asttypes
open Primitive
open Lambda
open Clambda
open Ctypedlambda
open Format


open Il
let str_class x = x.trnsp ^ "." ^ x.trnme


let rec prtypeinfo ppf = function
    TIint -> fprintf ppf "int"
  | TIchar -> fprintf ppf "char"
  | TIint32 -> fprintf ppf "int32"
  | TIint64 -> fprintf ppf "int64"
  | TInint -> fprintf ppf "nint"
  | TIfloat -> fprintf ppf "float"
  | TIbool -> fprintf ppf "bool"
  | TIunit -> fprintf ppf "unit"
  | TIstring -> fprintf ppf "string"
  | TIvoid -> fprintf ppf "void"
  | TIgenclosure -> fprintf ppf "genclos"
  | TIsharedclosure -> fprintf ppf "mclos" 
  | TIobject -> fprintf ppf "<obj>"
(*  | TIclosure cid -> Printf.sprintf "<C!%s>" (str_class cid) *)
  | TIarrow (args,res) -> fprintf ppf "(%a->%a)" (prlist "->") args prtypeinfo res
  | TIarray ti ->  fprintf ppf "%a[]" prtypeinfo ti
  | TIpureIL cid -> fprintf ppf "<IL:%s>" (str_class cid)
  | TInotimplemented x -> failwith ("to_il Not Impl "^x)
  | TIdontknow ->  fprintf ppf "<?>"

  | TIblock -> fprintf ppf "block"
  | TIexception -> fprintf ppf "exception"
  | TIlazy ti -> fprintf ppf "%a lazy_t" prtypeinfo ti
  | TIlist ti -> fprintf ppf "%a list" prtypeinfo ti
  | TIoption ti -> fprintf ppf "%a option" prtypeinfo ti
  | TIrecord id -> fprintf ppf "%s" (print_namedtype_path id)
  | TIvariant id -> fprintf ppf "%s" (print_namedtype_path id)
  | TItuple tilist -> fprintf ppf "(%a)" (prlist "*") tilist


and prlist sep ppf = function
    [t] -> fprintf ppf "%a" prtypeinfo t
  | t::q -> fprintf ppf "%a%s%a" prtypeinfo t sep (prlist sep) q
  | [] -> ()


let typedident_print display_types printer ppf tid = 
 if display_types then
   fprintf ppf "@[(%a:%a)@]" Ident.print (fst tid) printer (snd tid)
 else Ident.print ppf (fst tid)

let funlabel display_types printer ppf lbl =
  if display_types then
    fprintf ppf "@[%s:%a@]" lbl.opt prtypeinfo lbl.funtype
  else fprintf ppf "%s" lbl.opt


let rec ti_structured_constant printer ppf = function
  | TypTConst_base c -> Printlambda.structured_constant ppf (Const_base c) 
  | TypTConst_pointer i -> Printlambda.structured_constant ppf (Const_pointer i)
  | TypTConst_block(tag, []) ->
      fprintf ppf "[%i]" tag
  | TypTConst_block(tag, (sc1,ti)::scl) ->
      let sconsts ppf scl =
        List.iter (fun (sc,ti) -> fprintf ppf "@ %a:%a" (ti_structured_constant printer) sc printer ti) scl in
      fprintf ppf "@[<1>[%i:@ @[%a:%a%a@]]@]" tag (ti_structured_constant printer) sc1 printer ti sconsts scl
  | TypTConst_float_array fls -> Printlambda.structured_constant ppf (Const_float_array fls) 


let rec ulambda display_types printer ppf ul =
  if display_types then 
    fprintf ppf "@[(%a,%a)@]" (uterm display_types printer) ul.utlterm printer ul.utltype
  else uterm display_types printer ppf ul.utlterm 
and uterm display_types printer ppf = function
  | TypUvar id -> 
      Ident.print ppf id


  | TypUconst cst -> 
      if display_types then ti_structured_constant printer ppf cst 
      else Printlambda.structured_constant ppf (Ctypedlambda.to_ulambda_const cst)

  | TypUdirect_apply (fl,ul) -> 
      pp_open_box ppf 2 ;
      fprintf ppf "(direct:%a" (funlabel display_types printer) fl ; 
      List.iter (fun x -> pp_print_space ppf () ; ulambda display_types printer  ppf x) ul ;
      fprintf ppf ")" ;
      pp_close_box ppf ()

  | TypUgeneric_apply (u,ul) ->
      pp_open_box ppf 2 ;
      fprintf ppf "(apply"; pp_print_space ppf () ;
      ulambda display_types printer  ppf u ;
      List.iter (fun x -> pp_print_space ppf (); ulambda display_types printer  ppf x) ul ;
      fprintf ppf ")" ;
      pp_close_box ppf ()

  | TypUclosure (fdef,argdef) -> 
      let f (fl,ar,idl,u) = 
	pp_open_box ppf 2 ;
	fprintf ppf "(deffun(%a)(%d)" (funlabel display_types printer) fl ar ; pp_print_space ppf () ;
	pp_open_box ppf 2 ;
	fprintf ppf "(" ;
	( let rec it = function 
	    [] -> () 
	  | [x] -> typedident_print display_types printer ppf x ; pp_print_string ppf ")" ;
                     pp_print_space ppf ()
	  | x::l -> typedident_print display_types printer ppf x ; pp_print_space ppf () ; it l 
	  in it idl ) ;
	ulambda display_types printer  ppf u ;
	pp_print_string ppf ")" ;
	pp_close_box ppf () 
      in
      pp_open_box ppf 2 ;
      fprintf ppf "(closure(%d,%d)" (List.length fdef) (List.length argdef) ;
      List.iter (fun a -> pp_print_space ppf (); f a) fdef ;
      if argdef<>[] then pp_print_space ppf () ;
      List.iter (fun a -> pp_print_space ppf (); ulambda display_types printer  ppf a) argdef ;
      pp_print_string ppf ")";
      pp_close_box ppf ()

  | TypUoffset (u,n) -> 
      pp_open_box ppf 2 ;
      fprintf ppf "(offset(%d)" n ;
      pp_print_space ppf () ;
      ulambda display_types printer  ppf u ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | TypUlet (id,arg,body) ->
(*
      open_box 2;
      print_string "(let"; print_space();
      typedident_print display_types printer id; print_space(); 
      ulambda display_types printer  arg; print_space () ;
      ulambda display_types printer  body; print_string ")";
      close_box ()
*)
      pp_open_box ppf 2;
      pp_print_string ppf "(let"; pp_print_space ppf ();
      pp_open_hvbox ppf 1;
      pp_print_string ppf "(";
      pp_open_box ppf 2; typedident_print display_types printer ppf id; pp_print_space ppf (); 
      ulambda display_types printer  ppf arg; pp_close_box ppf ();
      letbody display_types printer ppf body;
      pp_print_string ppf ")";
      pp_close_box ppf ()

  | TypUletrec (idul,u) -> 
      let f (id,u) = 
        pp_print_space ppf () ;
	pp_open_box ppf 2 ;
	pp_print_string ppf "(" ;
	typedident_print display_types printer ppf id ; pp_print_space ppf () ;
	ulambda display_types printer  ppf u ;
	pp_print_string ppf ")" ;
	pp_close_box ppf ()
      in 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(letrec" ;
      List.iter f idul ;
      pp_print_space ppf () ;
      ulambda display_types printer  ppf u ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | TypUprim (p,ul) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(prim:" ; 
      Printlambda.primitive ppf p ;
      List.iter (fun u -> pp_print_space ppf (); ulambda display_types printer  ppf u) ul ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | TypUcatch (i,ids,u1,u2) -> 
      pp_open_box ppf 2;
      pp_print_string ppf "(catch"; pp_print_space ppf ();
      pp_print_int ppf i;pp_print_space ppf ();
      List.iter (fun id -> typedident_print display_types printer ppf id;pp_print_space ppf ()) ids;
      ulambda display_types printer  ppf u1; pp_print_break ppf 1 (-1);
      pp_print_string ppf "c-with"; pp_print_space ppf () ; 
      ulambda display_types printer  ppf u2;
      pp_print_string ppf ")";
      pp_close_box ppf ()
(* MOD BRUTALE RAF *)

  | TypUifthenelse (u1,u2,u3) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(if" ; pp_print_space ppf () ;
      ulambda display_types printer  ppf u1 ; pp_print_space ppf () ;
      pp_print_string ppf "then" ; pp_print_space ppf () ;
      ulambda display_types printer  ppf u2 ; pp_print_space ppf () ;
      pp_print_string ppf "else" ; pp_print_space ppf () ;
      ulambda display_types printer  ppf u3 ; 
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | TypUsequence (u1,u2) -> 
      pp_open_box ppf 2;
      pp_print_string ppf "(seq"; pp_print_space ppf ();
      ulambda display_types printer  ppf u1; pp_print_space ppf (); 
      sequence display_types printer ppf u2; pp_print_string ppf ")";
      pp_close_box ppf ()
(*
      open_box 2 ;
      print_string "(seq" ; print_space () ;
      ulambda display_types printer  u1 ; print_space () ; 
      ulambda display_types printer  u2; 
      print_string ")" ;
      close_box ()
*)

  | TypUwhile (u1,u2) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(while" ; pp_print_space ppf () ;
      ulambda display_types printer  ppf u1 ; pp_print_space ppf () ; 
      pp_print_string ppf "do" ; pp_print_space ppf () ; 
      ulambda display_types printer  ppf u2 ; 
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | TypUfor (id,u1,u2,df,u3) -> 
      pp_open_box ppf 2  ;
      pp_print_string ppf "(for" ; pp_print_space ppf () ;
      typedident_print display_types printer ppf id ; pp_print_space ppf () ;
      pp_print_string ppf "=" ; pp_print_space ppf () ;
      ulambda display_types printer  ppf u1 ; pp_print_space ppf () ;
      pp_print_string ppf 
        ( match df with Upto -> "to" | Downto -> "downto") ;
      pp_print_space ppf () ;
      ulambda display_types printer  ppf u2 ; pp_print_space ppf () ;
      pp_print_string ppf "do" ; pp_print_space ppf () ;
      ulambda display_types printer  ppf u3 ;
      pp_print_string ppf ")" ;
      pp_close_box ppf () 
      
  | TypUassign (id,u) ->
      pp_open_box ppf 2 ;
      pp_print_string ppf "(assign" ; pp_print_space ppf () ;
      typedident_print display_types printer ppf id ; pp_print_space ppf () ; 
      ulambda display_types printer  ppf u;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | TypUtrywith (u1,id,u2) ->  
      pp_open_box ppf 2 ;
      pp_print_string ppf "(try" ; pp_print_space ppf () ;
      typedident_print display_types printer ppf id ; pp_print_space ppf () ; 
      ulambda display_types printer  ppf u1; pp_print_space ppf () ; 
      pp_print_string ppf "t-with" ; pp_print_space ppf () ; 
      ulambda display_types printer  ppf u2 ; pp_print_space ppf () ; 
      pp_print_string ppf ")" ;
      pp_close_box ppf () 

  | TypUswitch (u,us) -> 
      pp_open_box ppf 1;
      pp_print_string ppf 
(*AMOD : us.us_checked *)
        "(switch ";

       (* (if us.us_checked then "(switch-checked " else "(switch ");
       *)
       ulambda display_types printer  ppf u; pp_print_space ppf ();
      pp_open_vbox ppf 0;
      let spc = ref false in
      for i=0 to (Array.length us.tus_index_consts)-1 do 
	if !spc then pp_print_space ppf () else spc := true;
        pp_open_hvbox ppf 1;
        let j = us.tus_index_consts.(i) in 
        fprintf ppf "case int %d : (%d)" i j ;
        pp_print_space ppf () ; ulambda display_types printer  ppf us.tus_actions_consts.(j) ;
        pp_close_box ppf ()
      done ;
      for i=0 to (Array.length us.tus_index_blocks)-1 do 
	if !spc then pp_print_space ppf () else spc := true;
        pp_open_hvbox ppf 1; 
	let j = us.tus_index_blocks.(i) in 
        fprintf ppf "case tag %d : (%d)" i j ;
        pp_print_space ppf () ; ulambda display_types printer  ppf us.tus_actions_blocks.(j) ;
        pp_close_box ppf ()
      done ;
      pp_print_string ppf ")"; pp_close_box ppf () ; pp_close_box ppf ()

  | TypUstaticfail (i,lu) -> pp_print_string ppf "<staticfail ";pp_print_int ppf i;
      List.iter (fun u -> pp_print_string ppf " !";ulambda display_types printer  ppf u) lu;
      pp_print_string ppf ">" (* AJOUT BRUTAL RAF *)
      
  | TypUsend (met, obj, largs) ->
      pp_open_box ppf 2;
      pp_print_string ppf "(send"; pp_print_space ppf ();
      ulambda display_types printer  ppf obj; pp_print_space ppf ();
      ulambda display_types printer  ppf met;
      List.iter (fun l -> pp_print_space ppf (); ulambda display_types printer  ppf l) largs;
      pp_print_string ppf ")";
      pp_close_box ppf ()



and sequence display_types printer ppf ul = match ul.utlterm with
    TypUsequence(l1, l2) ->
      sequence display_types printer ppf l1; pp_print_space ppf (); sequence display_types printer ppf l2
(*  | Ulet(id, arg, body) ->
      pp_open_box ppf 2;
      pp_print_string ppf "let"; pp_print_space ppf ();
      typedident_print display_types printer ppf id; pp_print_space ppf (); ulambda display_types printer  ppf arg;
      pp_close_box ppf ();
      pp_print_space ppf  ();
      sequence ppf body *)
  | l ->
      ulambda display_types printer ppf ul


and letbody display_types printer ppf ul = match ul.utlterm with
    TypUlet(id, arg, body) ->
      pp_print_space ppf ();
      pp_open_box ppf 2; typedident_print display_types printer ppf id; 
      pp_print_space ppf (); ulambda display_types printer  ppf arg;
      pp_close_box ppf ();
      letbody display_types printer ppf body
  | l ->
      pp_print_string ppf ")";
      pp_close_box ppf ();
      pp_print_space ppf ();
      ulambda display_types printer ppf ul









