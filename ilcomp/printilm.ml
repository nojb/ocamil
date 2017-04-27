(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

open Asttypes
open Primitive
open Lambda
open Il
open Ilm
open Clambda
open Format


(* !!!!!!!!! *)
let pclass ppf id =
  pp_print_string ppf (id.trnsp^"."^id.trnme)
let ploc ppf loc = 
  pp_print_string ppf loc
  
let func_id ppf lbl = 
  match lbl.ilinfo with
    None -> pp_print_string ppf lbl.opt
  | Some il -> pp_print_string ppf (il.ilns^"."^il.ilname) 

let locl_tid ppf  id = 
  ploc ppf id.lid ;
  pp_print_string ppf ":" ;
  Ilm.type_printer ppf id.ltype

let fild_tid ppf  id =
  pclass ppf (fst id.did) ;
  pp_print_string ppf ("::"^(snd id.did))  ;
  pp_print_string ppf ":" ;
  Ilm.type_printer ppf id.dtype 

let slocl_tid ppf  id = 
  ploc ppf id.lid ;
  pp_print_string ppf ":" ;
  Ilm.type_printer ppf id.ltype

let func_id ppf lbl = 
  match lbl.ilinfo with
    None -> pp_print_string ppf lbl.opt
  | Some il -> pp_print_string ppf (il.ilns^"."^il.ilname) 

let tconstant ppf = function
  | Tconst_null -> pp_print_string ppf "null"
  | Tconst_int n -> pp_print_int ppf n
  | Tconst_char c -> pp_print_char ppf c
  | Tconst_bool b -> pp_print_bool ppf b
  | Tconst_float s -> pp_print_string ppf s 
  | Tconst_bfloat s -> fprintf ppf "<%s>" s  (*AJOUT RAF *)
  | Tconst_string (_,s) -> fprintf ppf "\"%s\"" s 
(*  | Tconst_ctr n -> fprintf ppf "@@%d" n*)



let rec tlambda ppf  = function
  | Tconst cst ->  
      tconstant ppf cst 

  | Tdirect_apply (lb,tl) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(direct:" ;
      func_id ppf lb ;
      List.iter (fun x -> pp_print_space ppf (); tlambda ppf  x) tl ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tgeneric_apply (t,tl) ->
      pp_open_box ppf 2 ;
      pp_print_string ppf "(apply " ;
      tlambda ppf  t ;
      List.iter (fun x -> pp_print_space ppf (); tlambda ppf  x) tl ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tlet (id,arg,body) ->
      pp_open_box ppf 2 ;
      pp_print_string ppf "(let" ; pp_print_space ppf () ;
      slocl_tid ppf  id ; pp_print_space ppf () ; 
      tlambda ppf  arg ; pp_print_space ppf () ;
      tlambda ppf  body;
      pp_print_string ppf ")";
      pp_close_box ppf ()

  | Tletrec (idol,o) -> 
      let f (tid,o) = 
        pp_print_space ppf () ;
	pp_open_box ppf 2 ;
	pp_print_string ppf "(" ;
	locl_tid ppf  tid ; pp_print_space ppf () ;
	tlambda ppf  o ; 
	pp_print_string ppf ")" ;
	pp_close_box ppf ()
      in 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(letrec" ; pp_print_space ppf () ;
      List.iter f idol ; pp_print_space ppf () ;
      tlambda ppf  o ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tprim (CPlegacyprim op,ol) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(prim:" ; 
      Printlambda.primitive ppf op ;
      List.iter (fun o -> pp_print_space ppf (); tlambda ppf  o) ol ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()
  | Tprim (CPnewprim op,ol) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(Eprim:" ; 
      tprimitive ppf  op ;
      List.iter (fun o -> pp_print_space ppf (); tlambda ppf  o) ol ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tstaticfail (i,ol) ->
      pp_print_string ppf "<staticfail ";pp_print_int ppf i;
      List.iter (fun o -> pp_print_string ppf " !";tlambda ppf  o) ol;
      pp_print_string ppf ">"


  | Tcatch (i,ids,o1,o2) -> 
      pp_open_box ppf 2;
      pp_print_string ppf "(catch" ; pp_print_space ppf () ;
      pp_print_int ppf i;pp_print_space ppf ();
      pp_print_string ppf " (";
      pp_print_int ppf (List.length ids);
      pp_print_string ppf ") ";
      List.iter (fun id -> pp_print_string ppf id.lid;pp_print_space ppf ()) ids;
      tlambda ppf  o1 ; pp_print_space ppf () ;
      pp_print_string ppf "c-with";  pp_print_space ppf () ;
      tlambda ppf  o2; pp_print_string ppf")";
      pp_close_box ppf ()

  | Tifthenelse (o1,o2,o3) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(if" ; pp_print_space ppf () ;
      tlambda ppf  o1 ; pp_print_space ppf () ;
      pp_print_string ppf "then" ; pp_print_space ppf () ;
      tlambda ppf  o2 ; pp_print_space ppf () ;
      pp_print_string ppf "else" ; pp_print_space ppf () ;
      tlambda ppf  o3;
      pp_print_string ppf  ")" ;
      pp_close_box ppf ()

  | Tifvar (o1,o2,o3) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(ifvar" ; pp_print_space ppf () ;
      tlambda ppf  o1 ; pp_print_space ppf () ;
      pp_print_string ppf "then" ; pp_print_space ppf () ;
      tlambda ppf  o2 ; pp_print_space ppf () ;
      pp_print_string ppf "else" ; pp_print_space ppf () ;
      tlambda ppf  o3;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tsequence (o1,o2) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(seq" ; pp_print_space ppf () ;
      tlambda ppf  o1 ; pp_print_space ppf () ; 
      tlambda ppf  o2;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Twhile (o1,o2) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(while" ; pp_print_space ppf () ;
      tlambda ppf  o1 ; pp_print_space ppf () ; 
      tlambda ppf  o2 ;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tfor (id,t1,t2,df,t3) -> 
      pp_open_box ppf 2  ;
      pp_print_string ppf "(for" ; pp_print_space ppf  () ;
      locl_tid ppf  id ; pp_print_space ppf () ;
      pp_print_string ppf "=" ; pp_print_space ppf () ;
      tlambda ppf  t1 ; pp_print_space ppf () ;
      pp_print_string ppf 
         ( match df with Upto -> "to" | Downto -> "downto") ;
      pp_print_space ppf () ;
      tlambda ppf  t2 ; pp_print_space ppf () ;
      pp_print_string ppf "do" ; pp_print_space ppf () ;
      tlambda ppf  t3 ;
      pp_print_string ppf ")" ;
      pp_close_box ppf () 

  | Tassign (id,u) ->
      pp_open_box ppf 2 ;
      pp_print_string ppf "(assign" ; pp_print_space ppf () ;
      slocl_tid ppf  id ; pp_print_space ppf () ; 
      tlambda ppf  u;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tswitch (t,ct,ts) -> 
      pp_open_box ppf 1 ;
      pp_print_string ppf "(switch ";
      tlambda ppf  t ;
      Ilm.type_printer ppf ct;
      pp_print_space ppf () ;
      pp_open_vbox ppf 0 ;
      let spc = ref false in
      for i=0 to (Array.length ts.ts_actions)-1 do 
	if !spc then pp_print_space ppf () else spc := true ;
        pp_open_hvbox ppf 1 ;
        pp_print_string ppf "case "; 
	pp_print_int ppf ts.ts_indexes.(i) ;
        pp_print_string ppf ":" ; 
	pp_print_space ppf () ; 
	tlambda ppf  ts.ts_actions.(i) ;
        pp_close_box ppf  ()
      done ;
      pp_print_string ppf ")"; 
      pp_close_box ppf () ; 
      pp_close_box ppf ()

  | Ttrywith (u1,id,u2) ->  
      pp_open_box ppf 2 ;
      pp_print_string ppf "(try" ; pp_print_space ppf () ;
      locl_tid ppf  id ; pp_print_space ppf () ; 
      tlambda ppf  u1; pp_print_space ppf () ; 
      pp_print_string ppf "t-with" ; pp_print_space ppf () ; 
      tlambda ppf  u2 ; pp_print_space ppf () ; 
      pp_print_string ppf  ")" ;
      pp_close_box ppf () 

  | Tsend (t1,t2,tl) -> 
      pp_open_box ppf 2 ;
      pp_print_string ppf "(send" ; pp_print_space ppf () ;
      tlambda ppf  t1 ;pp_print_space ppf () ;
      tlambda ppf  t2 ;pp_print_space ppf () ;
      List.iter (fun t -> tlambda ppf  t ; pp_print_space ppf ()) tl;
      pp_print_string ppf ")" ;
      pp_close_box ppf ()

  | Tlocal id -> 
      pp_print_string ppf "loc:" ;
      locl_tid ppf  id

  | Targument id ->  
      pp_print_string ppf "arg:" ;
      slocl_tid ppf  id

  | Tnop ->
      pp_print_string ppf "NOP"


and tprimitive ppf  = function 
  | TP_get_builtin id -> fprintf ppf "get_builtin:%s" id
  | TP_get_field id -> pp_print_string ppf "G-FIELD " ; fild_tid ppf  id
  | TP_set_field id -> pp_print_string ppf "S-FIELD " ; fild_tid ppf  id
  | TP_get_global id -> pp_print_string ppf "GLOBAL " ; pp_print_string ppf id

  | TP_mktop id -> pp_print_string ppf "mktop:" ; pclass ppf id 
  | TP_mkenv id -> pp_print_string ppf "mkenv:" ; pclass ppf id 
  | TP_mkrec id -> pp_print_string ppf "mkrec:" ; pclass ppf id 
  | TP_mkclos id -> pp_print_string ppf "mkclos:" ; pclass ppf id 

  | TP_buildobject (id,_,_) -> pp_print_string ppf "buildobject:" ; pclass ppf id

  | TP_raise ty -> pp_print_string ppf "raise:";Ilm.type_printer ppf ty
  | TP_set_block n -> fprintf ppf "S-ARRAY.%d " n
  | TP_get_block n -> fprintf ppf "G-ARRAY.%d " n
  | TP_eq -> pp_print_string ppf "EQ"
  | TP_neq -> pp_print_string ppf "NEQ"
  | TP_string_length ct ->  
      pp_print_string ppf "strlength(" ;
      Ilm.type_printer ppf ct ; pp_print_string ppf ")" 
  | TP_string_set (ct,safemode) ->
      pp_print_string ppf "strset(" ;
      Ilm.type_printer ppf ct ; pp_print_string ppf ")" 
  | TP_string_ref (ct,safemode) ->
      pp_print_string ppf "strref(" ;
      Ilm.type_printer ppf ct ; pp_print_string ppf ")" 
  | TP_stringcomp cmp -> fprintf ppf "strcmp(%s)" 
      (match cmp with Ceq -> "equal" | Cneq -> "nequal" | Cgt -> ">" 
	 | Clt -> "<" | Cle -> "<=" | Cge -> ">=") 

(*  | TP_mkarray n -> fprintf ppf "mkarray(%d):" n  *)
  | TP_cast (src,dst) ->
      pp_print_string ppf "cast(" ;
      Ilm.type_printer ppf src ; pp_print_string ppf "->" ;
      Ilm.type_printer ppf dst ; pp_print_string ppf ")" 
  | TP_pushdummy typ -> 
      pp_print_string ppf "pushdummy(" ;
      Ilm.type_printer ppf typ ; pp_print_string ppf ")" 
(*  | TP_bittest s -> fprintf ppf "bittest(%s)" s  *)
  | TP_convint bi -> fprintf ppf "convint(%s)" (match bi with Lambda.Pnativeint -> "i" | Lambda.Pint32 -> "i4" | Lambda.Pint64 -> "i8")
  | TP_offsetref n -> fprintf ppf "offsetref(%d):" n 
  | TP_pushint n -> fprintf ppf "pushint(%d):" n 

 

let tfundec ppf  fd = 
  pp_open_box ppf 2 ;
  if fd.tfd_cur then pp_print_string ppf "*** CURRY " ; 
  ( match fd.tfd_sts with 
      TFStop -> pp_print_string ppf "*** FUNCTION : " 
    | TFSenv fl -> 
        pp_print_string ppf "*** ENVT [" ;
        Utils.iter_except_last (fild_tid ppf ) (pp_print_space ppf) fl ;
        pp_print_string ppf "] FUNCTION : " 
    | TFSrec id -> 
        pp_print_string ppf "*** REC " ; 
        pclass ppf id ; 
        pp_print_string ppf " FUNCTION : " ) ;
  func_id ppf fd.tfd_id ;   
  pp_print_space ppf () ;
  pp_open_box ppf 2; pp_print_string ppf "[" ;
  Utils.iter_except_last (slocl_tid ppf ) (pp_print_space ppf) fd.tfd_var ;
  pp_print_string ppf "] -> " ; Ilm.type_printer ppf fd.tfd_rt ;
  pp_close_box ppf () ; pp_print_space ppf () ;
  tlambda ppf  fd.tfd_exe ;
  pp_print_newline ppf () ;
  pp_print_newline ppf () ;
  pp_close_box ppf () 




let tclassdec ppf  cd = 
  pp_open_box ppf 2 ;
  pp_print_string ppf "*** SHARED CLOSURE : " ;
  pclass ppf cd.tcd_id ;
  pp_print_space ppf () ;
  pp_open_box ppf 2; pp_print_string ppf "[" ;
  Utils.iter_except_last (locl_tid ppf ) (pp_print_space ppf) cd.tcd_fld ;
  pp_print_string ppf "]" ; pp_close_box ppf () ; pp_print_space ppf () ;
  pp_print_newline ppf () ;
  pp_print_newline ppf () ;
  pp_close_box ppf () 


let tunitdec ppf  ud = 
  pp_print_string ppf 
    "                      ######  TLAMBDA Debug Info ######" ;
  pp_print_newline ppf () ;
  pp_print_newline ppf () ;
  List.iter (tclassdec ppf ) ud.tud_cd ;
  List.iter (tfundec ppf ) ud.tud_fd ;
  pp_open_box ppf 2 ;
  fprintf ppf "*** UNIT " ;
  pp_print_space ppf () ;
  pp_print_string ppf ud.tud_id ;
  fprintf ppf "[%d]" ud.tud_size; 
  pp_print_space ppf () ;
  tlambda ppf  ud.tud_start ;
  pp_print_string ppf ")" ;
  pp_print_newline ppf () ;
  pp_print_newline ppf () ;
  pp_close_box ppf ()
