(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: rebuildtypes.ml,v 1.1 2006/10/16 12:45:55 montela Exp $ *)


open Asttypes
open Primitive
open Lambda
open Clambda 
open Ctypedlambda
open Compilenv
open Il

(* for purpose of type inference, runtime type is a graph 
   to share type into several values (not need type variable) *)

type ttype = 
  | TTint | TTbool | TTchar | TTstring | TTfloat | TTvoid  | TTint64 | TTnint
  | TTblock  | TTbox
  | TTclos
  | TTclass of typeref                 (* user class *)
  | TTunk                              (* not instanciated type *)
  | TTsame of treftype                 (* shared type *)

and treftype = ttype ref 


let str_class x = x.trnsp ^ "." ^ x.trnme

open Format 
let rec print ppf = function

  | TTchar -> pp_print_string ppf "char"
  | TTbool -> pp_print_string ppf "bool"
  | TTvoid       -> pp_print_string ppf "void"
  | TTint        -> pp_print_string ppf "int"
  | TTint64        -> pp_print_string ppf "int64"
  | TTnint        -> pp_print_string ppf "Nint"
  | TTfloat      -> pp_print_string ppf "float"
  | TTstring     -> pp_print_string ppf "<String>"
  | TTclos       -> pp_print_string ppf "<Closure>" 
  | TTclass id   -> pp_print_string ppf "<" ; 
                    pp_print_string ppf (str_class id) ;
                    pp_print_string ppf ">"
  | TTblock    -> pp_print_string ppf "<block>"  
  | TTbox        -> pp_print_string ppf "<box>"   
  | TTunk        -> pp_print_string ppf "<?>"   
  | TTsame ot    -> print ppf !ot

let rec to_string = function
  | TTchar -> "char"
  | TTbool -> "bool"
  | TTvoid        -> "void"
  | TTint         -> "int"
  | TTint64         -> "int64"
  | TTnint         -> "Nint"
  | TTfloat       -> "float"
  | TTstring      -> "<String>"
  | TTclos        -> "<Closure>" 
  | TTclass id    ->  "<" ^ (str_class id) ^ ">" 
  | TTblock       -> "<block>" 
  | TTbox         -> "<box>"   
  | TTunk         -> "<?>"   
  | TTsame ot     ->  "*"^(to_string !ot)

let print ppf tr = print ppf !tr
let to_string tr = to_string !tr

let rec freeze_type t = match !t with 
  | TTsame t1 -> freeze_type t1 
  | TTunk -> t := TTbox
  | _ -> ()


(* unification of runtime type *)

let simple rt = 
  match !rt with 
    TTsame {contents=TTsame t} -> rt := TTsame t 
  | _ -> () 

exception TType_NU

(* attempts to unify types, raise exception if not possible *)
let rec unify t1 t2 = 
  if t1!=t2 then 
    match (!t1, !t2) with 
    | TTsame t , _ -> unify t t2 ; simple t1 
    | _ , TTsame t -> unify t1 t ; simple t2 
(*    | _ , _ when t1 = t2 -> t1 := TTsame t2 ; simple t1 *)
    | TTunk , _ -> t1 := TTsame t2 ; simple t1 
    | _ , TTunk -> t2 := TTsame t1 ; simple t2
    | _ , _ -> raise TType_NU
   

let rec keepunder t1 t2 =
  match (!t1,!t2) with
    | TTsame t , _ -> keepunder t t2
    | _ , TTsame t -> keepunder t1 t
    | TTbox , TTunk -> true
    | _ , TTbox -> true
    | _ , _ -> false

let sub_unify t1 t2 = 
  if keepunder t1 t2 then t1
  else try 
    unify t1 t2; t1
  with TType_NU -> t1

let unify_common t1 t2 = 
  if keepunder t1 t2 then t2
  else if keepunder t2 t1 then t1
  else (unify t1 t2;t2)
  
let symmetric_unify tl tc = 
  try 
    sub_unify (List.fold_left unify_common (ref TTunk) tl) tc
  with TType_NU -> tc 
    
let symmetric_unify2 t1 t2 tc = 
  symmetric_unify [t1;t2] tc

let treftype_of_il = function 
    Tint32 -> ref TTint 
  | Tint64 -> ref TTint64 
  | Tnint -> ref TTnint
  | Tchar -> ref TTchar
  | Tbool -> ref TTbool
  | Tfloat64 -> ref TTfloat
  | Tvoid -> ref TTvoid 
  | Tstring -> ref TTstring
  | Tclass tr when tr.trnsp = "System.Text" && tr.trnme ="StringBuilder" -> ref TTstring 
  | Tvector Tchar -> ref TTstring
  | Tclass tr when tr.trnsp = "CamIL" && tr.trnme ="Closure" -> ref TTclos 
  | Tclass tr when tr.trnsp = "CamIL" && tr.trnme ="Exception" -> ref TTunk (*Traise true*)
  | Tobject -> ref TTbox
  | Tclass x -> ref (TTclass x)
  | Tvector _ -> ref TTblock
  | _ -> Utils.bug "Rebuildtypes" "treftype_of_il" 

open Clambda

let rec to_typeinfo t = match !t with
  | TTint -> TIint
  | TTbool -> TIbool
  | TTchar -> TIchar
  | TTstring -> TIstring
  | TTfloat -> TIfloat
  | TTvoid -> TIvoid
  | TTint64 -> TIint64
  | TTnint -> TInint
  | TTblock -> TIblock
  | TTbox -> TIobject
  | TTclos -> TIgenclosure
  | TTclass il -> TIpureIL il
  | TTunk -> TIobject
  | TTsame t2 -> to_typeinfo t2


(* environment for compiling *)

type envt = { 
    variables : (Ident.t * treftype) list ;  
    argenv : Ident.t option; (* used for recognizing function environments (see Pfield) *)
  }

(* manage variables *)

let add_new_variable env id ti = 
  { env with variables = (id,ti) :: env.variables }

let find_variable env id = List.assoc id env.variables 

(* AJOUT RAF pour le typage des Catch et Staticfail nouveaux *)
(* Il faut un environnement qui associe à chaque numero de catch,
 la liste d'identificateurs pouvant recevoir les valeurs transmises par un Staticfail *)

let catch_env = ref ([] : (int * ((Ident.t * treftype) list) ) list)
let label_env = ref ([] : (function_label * (treftype * treftype list)) list)

(* initialize environment *)

let env_init() = 
  label_env := [];
  catch_env := [];
  {
    variables = [] ;  
    argenv = None ;
  }

let add_catch_env n tid_list = 
  try (ignore (List.assoc n !catch_env);Utils.bug "Rebuildtypes" "add_catch_env")
  with Not_found -> catch_env := (n,tid_list)::(!catch_env)

let get_catch_env n = List.assoc n !catch_env
let remove_catch_env n = catch_env := List.remove_assoc n !catch_env

let add_func lbl (rt,args) = label_env := (lbl,(rt,args)) :: (!label_env)
let find_func lbl = List.assoc lbl !label_env
let available_fundec lbl = List.mem_assoc lbl !label_env

let freeze_fun env (flbl,ar,tids,tu) = 
  let (rt,args) = find_func flbl in
    List.iter freeze_type args;
    freeze_type rt;
    List.iter (fun (id,tid) -> freeze_type tid) tids;
    freeze_type tu.utltype;
    let real_args = if abs ar <> List.length tids then fst (Utils.last_list tids) else tids in
    let targs = List.map (fun (id,tid) -> to_typeinfo tid) real_args in
      flbl.funtype <-  TIarrow((if ar < 0 then [TItuple targs] else targs) , to_typeinfo tu.utltype)

(* find a unit name in .cmx information *)

let find_unit id = (* utilisé seulement pour la primitive get_global *)
  let cui=Compilenv.get_current_unit () in 
  let modname = Ident.name id in 
  (* REM : le deuxieme cas sert au toplevel *)
  if (modname=cui.ui_name || modname="$specialuse_toplevel") 
  then Naming.get_unit_id ()
  else let (_,y,_) = 
     if (!Clflags.verbose) then (print_string "----------"; print_newline());
     List.find (fun (x,_,_) -> 
		  if (!Clflags.verbose) then (print_string x; print_string "<-->"; 
                               print_string modname; print_string "|"; print_newline());
		  x=modname) 
       (List.rev cui.ui_imports_cmx) (*TEMPO patch dégueu pour le toplevel *) in
(*       if y = [] then Utils.bug "Cannot extract globals from the following cmx :" modname else*)
    y.trnsp

                           (*****************)

let type_of_bigint = function
  | Pint32 -> ref TTint
  | Pint64 -> ref TTint64
  | Pnativeint -> ref TTnint

(* translate structured_constant *)
let rec structured_constant rt = function 
    Const_base c -> 
      let ti = match c with
	  Const_int _ -> ref TTint
	| Const_char _ -> ref TTchar
	| Const_string _ -> ref TTstring
	| Const_float _ -> ref TTfloat
      in
	TypTConst_base c , (sub_unify ti rt)
  | Const_pointer n ->
      TypTConst_pointer n , (sub_unify (ref TTint) rt)
  | Const_block (n,scl) -> 
      let arg_block = List.map (structured_constant (ref TTunk)) scl in
	TypTConst_block(n,arg_block) , (sub_unify (ref TTblock) rt)
  | Const_float_array sl ->
      TypTConst_float_array sl , (sub_unify (ref TTblock) rt)

let rec ulambda env rt = function
  | Uconst c -> 
      let tu,typ = structured_constant rt c in
	build_uterm (TypUconst tu) typ

  | Ugeneric_apply (u,ul) -> 
      let tu = ulambda env (ref TTclos) u
      and tul = List.map (fun u -> let t = ref TTunk in ulambda env t u) ul in
	build_uterm (TypUgeneric_apply(tu,tul)) rt

  | Uswitch (u,us) -> 
      let t = ref (if Array.length us.us_index_blocks = 0 then TTint else TTblock) in 
      let tu = ulambda env t u in 
      let annotate_args u = let rtu = ref TTunk in (ulambda env rtu u,rtu) in
      let tl = List.map annotate_args ((Array.to_list us.us_actions_consts)@(Array.to_list us.us_actions_blocks)) in
      let l1,l2 = Utils.cut_list (Array.length us.us_actions_consts) (List.map fst tl) in
      let tus = 
	{ tus_index_consts = us.us_index_consts ;
          tus_actions_consts = Array.of_list l1;
          tus_index_blocks = us.us_index_blocks ;
          tus_actions_blocks = Array.of_list l2 
	} in
	build_uterm (TypUswitch(tu,tus)) (symmetric_unify (List.map snd tl) rt)

(* no need to be so complicated 
  | Uswitch (u,us) -> 
      let annotate_args u = 
	let rtu = ref TTunk in
	  (ulambda env rtu u,rtu) in
      let t = ref (if Array.length us.us_index_blocks = 0 then TTint else TTblock) in 
      let tu = ulambda env t u in 
      let tl = List.map annotate_args ((Array.to_list us.us_actions_consts)@(Array.to_list us.us_actions_blocks)) in
      let l1,l2 = Utils.cut_list (Array.length us.us_actions_consts) (List.map fst tl) in
      let tus = 
	{ tus_index_consts = us.us_index_consts ;
          tus_actions_consts = Array.of_list l1;
          tus_index_blocks = us.us_index_blocks ;
          tus_actions_blocks = Array.of_list l2 
	} in
	build_uterm (TypUswitch (tu,tus)) (nary_sub_unify (rt::(List.map (fun x -> (fst x).utltype) tl)))
	*)	  

  | Ustaticfail (i, ul) ->
      begin
	try 
	  let tul = List.map2 (fun u id -> ulambda env (snd id) u) ul (get_catch_env i) in
	    build_uterm (TypUstaticfail (i,tul)) rt
	with Not_found -> Utils.bug "Rebuildtypes" "Typage de Ustaticfail"
      end

  | Ucatch (i,ids,u1,u2) ->
      let tid_list = List.map (fun id -> (id,ref TTunk)) ids in
	add_catch_env i tid_list;
	let rt1 = ref TTunk in
	let tu1 = ulambda env rt1 u1 in
	let env2 = List.fold_left (fun env (id,ti) -> add_new_variable env id ti) env tid_list in 
	  remove_catch_env i;
	  let rt2 = ref TTunk in
	  let tu2 = ulambda env2 rt2 u2 in
	    build_uterm (TypUcatch (i,tid_list,tu1,tu2)) (symmetric_unify2 rt1 rt2 rt)

  | Uifthenelse (u1,u2,u3) -> 
      let tu1 = ulambda env (ref TTbool) u1 in
      let rt2 = ref TTunk in
      let rt3 = ref TTunk in
      let tu2 = ulambda env rt2 u2 in
      let tu3 = ulambda env rt3 u3 in
	build_uterm (TypUifthenelse(tu1,tu2,tu3))  (symmetric_unify2 rt2 rt3 rt)

  | Usequence (u1,u2) -> 
      let tu1 = ulambda env (ref TTunk) u1 in
      let rt2 = ref TTunk in
      let tu2 = ulambda env rt2 u2 in 
	build_uterm (TypUsequence(tu1,tu2)) (sub_unify rt2 rt)
      
  | Uwhile (u1,u2) -> 
      let tu1 = ulambda env (ref TTbool) u1 in
      let rt2 = ref TTvoid in 
      let tu2 = ulambda env rt2 u2 in
	build_uterm (TypUwhile(tu1,tu2)) (sub_unify rt2 rt)

 | Usend (u1, u2, []) -> assert false
(*     let tu1 = ulambda env (ref TTunk) u1 in
     let tu2 = ulambda env (ref TTunk) u2 in
       unify_box (ref TTobj) rt (Tsend(tu1,tu2,[])) *)

 | Usend (u1,u2,ul) -> assert false
(*     ulambda env rt (Ugeneric_apply(Usend(u1,u2,[]),ul)) *)


  | Uassign (id,u) ->
      begin try 
          let tid = find_variable env id in
          let tu = ulambda env tid u in 
	    build_uterm (TypUassign ((id,tid),tu)) (sub_unify (ref TTvoid) rt)
        with Not_found -> Utils.bug "Rebuildtypes.ulambda" "Uassign"
      end

  | Udirect_apply (flbl,ul) -> 
      begin try  
	if available_fundec flbl then  (* true for functions defined in the current implementation file AND already compiled *)
	  let (fret,argt) = find_func flbl in 
	  let tul = List.map2 (ulambda env) argt ul in
	    build_uterm (TypUdirect_apply(flbl,tul)) (sub_unify fret rt)
	else 
	  begin
	    if flbl.ilinfo=None then (List.iter (fun (lbl,_) -> Printf.printf "Available funct: %s\n" lbl.opt) (List.rev !label_env);Utils.bug "Rebuildtypes.ulambda.direct_apply" flbl.opt)
	    else
	      let cp = Utils.someof flbl.ilinfo in
	      let tul = List.map2 (fun (x,_) u -> ulambda env (treftype_of_il x) u) cp.ilsig ul in 
		build_uterm (TypUdirect_apply(flbl,tul)) (sub_unify (treftype_of_il cp.ilrt) rt)
	  end 
      with Not_found -> Utils.bug "Rebuildtypes.ulambda" "Udirect_apply" 
      end

      
  | Uvar id -> 
      if !Clflags.verbose then (Printf.printf "Uvar %s\n" (Ident.name id);flush stdout);    
      begin try 
	let tid = find_variable env id in 
	  build_uterm (TypUvar id) (sub_unify tid rt)
      with Not_found ->
        Utils.bug "Rebuildtypes.ulambda Uvar" (Ident.unique_name id)
      end

  | Ufor (id,u1,u2,df,u3) -> 
      let tint = ref TTint in 
      let tu1 = ulambda env tint u1
      and tu2 = ulambda env tint u2 in
      let env2 = add_new_variable env id tint in 
      let tu3 = ulambda env2 (sub_unify (ref TTvoid) rt) u3 in 
	build_uterm (TypUfor ((id,tint),tu1,tu2,df,tu3)) rt

  | Utrywith (u1,id,u2) ->
     let rt1 = ref TTunk in
     let tu1 = ulambda env rt1 u1 in 
     let tid = ref TTblock in
     let rt2 =  ref TTunk in
     let env2 = add_new_variable env id tid in 
     let tu2 = ulambda env2 rt2 u2 in 
       build_uterm (TypUtrywith(tu1,(id,tid),tu2)) (symmetric_unify2 rt1 rt2 rt)

  (* default for primitive *)
  | Uprim (p,ul) -> 
      (if !Clflags.verbose then (print_string "Uprim\n";flush stdout));
      primitive env rt p ul

  | Uclosure(fl,ul) -> 
      let retype_closure (flbl,ar,idl,u) = 
	let tidl = List.map (fun id -> let tid = ref TTunk in (id,tid)) idl in
	let fret = ref TTunk in
	  add_func flbl (fret,List.map snd tidl); 
	  fun () -> 
	    let env2 = List.fold_left (fun e (id,tid) -> add_new_variable e id tid) env tidl in
	     let env3 = if abs ar <> List.length idl then 
	       let  _ , envid = Utils.last_list idl in {env2 with argenv = Some envid} 
	     else env2 in
	     let tu = ulambda env3 fret u in
	       (flbl,ar,tidl,tu)
      in
      let tul = List.map (fun u -> let t = ref TTunk in ulambda env t u) ul in
      let secondsteps = List.map retype_closure fl in
      let tfl = List.map (fun f -> f()) secondsteps in
	List.iter (freeze_fun env) tfl;
	build_uterm (TypUclosure(tfl,tul)) (sub_unify (ref TTclos) rt)

(* no other refinments on finding the closure type !!! *)

  | Ulet (id,u1,u2) -> 
      let tid = ref TTunk in
      let tu1 = ulambda env tid u1 in
      let env2 = add_new_variable env id tid in
      let rt2 = ref TTunk in 
      let tu2 = ulambda env2 rt2 u2 in
	build_uterm (TypUlet ((id,tid),tu1,tu2)) (sub_unify rt2 rt)


  (* variables recursive definitions : create several names *)
  | Uletrec (decl,u) -> 
      let tidl = List.map (fun (id,_) -> let tid = ref TTunk in (id,tid)) decl in
      let env2 = List.fold_left (fun e (id,tid) -> add_new_variable e id tid) env tidl in
      let tdecl = List.map2 (fun (_,u) tid -> (tid,ulambda env2 (snd tid) u)) decl tidl in
      let rtu = ref TTunk in 
      let tu = ulambda env2 rtu u in
	build_uterm (TypUletrec(tdecl,tu)) (sub_unify rtu rt)
		       
  | Uoffset (u, n) -> 
      let tu = ulambda env (ref TTclos) u in (* clos for the offset argument *)
	build_uterm (TypUoffset(tu,n)) (sub_unify (ref TTclos) rt) (* clos for the offset result *)

(* no other refinments on finding the offset type !!! *)

and primitive env rt p ul = match p,ul with 

  | Pgetglobal id , [] -> build_uterm (TypUprim(p,[])) (sub_unify (ref TTblock) rt)
(*      begin try 
	let name = Ident.name id in
	  if List.mem name ["Failure";"Assert_failure";"Invalid_argument";
			    "Match_failure";"Not_found";"End_of_file";
			    "Out_of_memory";"Stack_overflow";"Sys_error";
			    "CLIinteraction.ManagedException"] then  
	    assert false (* builtin exception *) 
	  else build_uterm (TypUprim(p,[])) (sub_unify (ref TTblock) rt)
      with Not_found -> Utils.bug "primitive" "getglobal"
      end
*)
      
(* Pfields are used on closures environments too: adress this case first *)

  | Pfield n , [Uvar id as u] when env.argenv<>None && Utils.someof env.argenv = id -> 
      let tu = ulambda env (ref TTclos) u in
	build_uterm (TypUprim(p,[tu])) rt (* enhancement: sub_unify currentclosure[n] from some environnment with rt *)

  | (Pfield n | Pfldtag(n,_)) , [u] ->
      let tu = ulambda env (ref TTblock) u in
	build_uterm (TypUprim(p,[tu])) rt

(* no other refinments on finding the field type !!! *)

  | Psetfield (n,_) , [u1;u2] -> 
      let tu1 = ulambda env (ref TTblock) u1 in
      let tu2 = ulambda env (ref TTunk) u2 in
	build_uterm (TypUprim(p,[tu1;tu2])) (sub_unify (ref TTvoid) rt)

  | Pmakeblock (n,_) , ul  -> 
      let tul = List.map (fun u -> let t = ref TTunk in ulambda env t u) ul in
	build_uterm (TypUprim(p,tul)) (sub_unify (ref TTblock) rt)

  | Pccall pd , ul ->
      let ild = 
	try Utils.someof pd.Primitive.prim_IL 
	with _ -> Utils.bug "no primitive here" pd.Primitive.prim_name in
     let arg = match List.map (fun ilt -> treftype_of_il (fst ilt)) ild.Primitive.ilprim_sig with [] -> [ref TTvoid] | l -> l in 
     let ret = treftype_of_il ild.Primitive.ilprim_rt in
     let tul = List.map2 (ulambda env) arg ul in
       build_uterm (TypUprim(p,tul)) (sub_unify ret rt)

  | Praise , [u] -> 
      let tu = ulambda env (ref TTblock) u in 
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTunk) rt) (* avant : TTraise true *)

  | (Psequand | Psequor) , [u1;u2] -> 
      let tbool = ref TTbool in
      let tu1 = ulambda env tbool u1 in
      let tu2 = ulambda env tbool u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify tbool rt)

  | Pnot , [u] ->
      let tbool = ref TTbool in
      let tu = ulambda env tbool u in
	build_uterm (TypUprim (p,[tu])) (sub_unify tbool rt)

  | Pnegint , [u] -> 
      let tint = ref TTint in
      let tu = ulambda env tint u in
	build_uterm (TypUprim (p,[tu])) (sub_unify tint rt)
 
  | p , [u1;u2] when List.mem p 
      [ Paddint ; Psubint ; Pmulint ; Pdivint ; Pmodint ; Pandint ; 
        Porint ; Pxorint ; Plslint ; Plsrint ; Pasrint ] ->
      let tint = ref TTint in
      let tu1 = ulambda env tint u1 in
      let tu2 = ulambda env tint u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify tint rt)

  | Pintcomp _ as p , [u1;u2] ->
      let tint = ref TTunk in (* because it can be used for pointers !!! *)
      let tu1 = ulambda env tint u1 in
      let tu2 = ulambda env tint u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTbool) rt)


  | Pnegfloat , [u] -> 
      let tfloat = ref TTfloat in 
      let tu = ulambda env tfloat u in
	build_uterm (TypUprim (p,[tu])) (sub_unify tfloat rt)

  | p , [u1;u2] 
    when List.mem p [ Paddfloat ; Psubfloat ; Pmulfloat ; Pdivfloat ] ->
      let tfloat = ref TTfloat in 
      let tu1 = ulambda env tfloat u1 in
      let tu2 = ulambda env tfloat u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTfloat) rt)
  
  | Pfloatcomp _  , [u1;u2] ->  
      let tfloat = ref TTfloat in 
      let tu1 = ulambda env tfloat u1 in
      let tu2 = ulambda env tfloat u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTbool) rt)

  | Pstringlength , [u] -> 
      let tu = ulambda env (ref TTstring) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTint) rt)

  | (Pstringrefu | Pstringrefs) , [u1;u2] ->
      let tu1 = ulambda env (ref TTstring) u1 in
      let tu2 = ulambda env (ref TTint) u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTchar) rt)

  | (Pstringsetu | Pstringsets) , [u1;u2;u3] ->
      let tu1 = ulambda env (ref TTstring) u1 in
      let tu2 = ulambda env (ref TTint) u2 in
      let tu3 = ulambda env (ref TTchar) u3 in
	build_uterm (TypUprim (p,[tu1;tu2;tu3])) (sub_unify (ref TTvoid) rt)

  | Pidentity , [u] ->
      let rtu = ref TTunk in 
      let tu = ulambda env rtu u in
	build_uterm (TypUprim (p,[tu])) (sub_unify rtu rt)

  | Pignore , [u] -> 
      let tu = ulambda env (ref TTunk) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTvoid) rt)

  | Poffsetref n , [u] -> 
      let tu = ulambda env (ref TTblock) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTvoid) rt)

  | Poffsetint n , [u] -> 
      let tu = ulambda env (ref TTint) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTint) rt)

  | Pmakearray _ , ul -> 
      let tul = List.map (fun u -> let t = ref TTunk in ulambda env t u) ul in
	build_uterm (TypUprim (p,tul)) (sub_unify (ref TTblock) rt)

  | Parraylength _, [u] -> 
      let tu = ulambda env (ref TTblock) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTint) rt)

  | (Parrayrefu _ | Parrayrefs _) , [u1;u2] -> 
      let tu1 = ulambda env (ref TTblock) u1 in
      let tu2 = ulambda env (ref TTint) u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTunk) rt)

  | (Parraysetu _ | Parraysets _) , [u1;u2;u3] ->
      let tu1 = ulambda env (ref TTblock) u1 in
      let tu2 = ulambda env (ref TTint) u2 in
      let tu3 = ulambda env (ref TTunk) u3 in
	build_uterm (TypUprim (p,[tu1;tu2;tu3])) (sub_unify (ref TTvoid) rt)

  | (Pil "eq" | Pil "neq") , [u1;u2]  -> 
      let tunk = ref TTunk in
      let tu1 = ulambda env tunk u1 in
      let tu2 = ulambda env tunk u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTbool) rt)

  | Pil s , [u1;u2] when List.mem s ["str_equal";"str_nequal";"str_lessthan";"str_greaterthan";"str_lessequal";"str_greaterequal"]  -> 
      let tstr = ref TTstring in
      let tu1 = ulambda env tstr u1 in
      let tu2 = ulambda env tstr u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTbool) rt)

(*
  | Pil x , [u] when String.length x >= 7 && String.sub x 0 7 = "bittest" ->
      let orpat = String.sub x 7 ((String.length x)-7) in
      let tu = ulambda env (ref TTint) u in
      unify_box (ref TTint) rt (Tnewprim (TP_bittest orpat,[tu]))
*)

  | Pisout , [u1;u2] -> 
     let tu1 = ulambda env (ref TTint) u1
     and tu2 = ulambda env (ref TTint) u2 in 
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTbool) rt)

  | Pisint, [u] ->
      let tu = ulambda env (ref TTunk) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTbool) rt)

  | Pfloatfield n , [u] ->
      let tu = ulambda env (ref TTblock) u in 
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTfloat) rt)
	
  | Psetfloatfield n  , [u1;u2] -> 
      let tu1 = ulambda env (ref TTblock) u1 in
      let tu2 = ulambda env (ref TTfloat) u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTvoid) rt)
      
  | Pil s , _ -> Utils.bug "Rebuildtypes.primitive Pil " s

  | Pbintofint bi , [u] -> 
      let tu = ulambda env (ref TTint) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (type_of_bigint bi) rt)

  | Pintofbint bi , [u] -> 
      let tu = ulambda env (type_of_bigint bi) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (ref TTint) rt)

  | Pnegbint bi, [u] ->
      let tbint = type_of_bigint bi in
      let tu = ulambda env tbint u in
	build_uterm (TypUprim (p,[tu])) (sub_unify tbint rt)

  | (Paddbint bi | Psubbint bi | Pmulbint bi | Pdivbint bi 
    | Pmodbint bi | Pandbint bi | Porbint bi | Pxorbint bi) , [u1;u2] -> 
      let tbint = type_of_bigint bi in
      let tu1 = ulambda env tbint u1 in
      let tu2 = ulambda env tbint u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify tbint rt)

  | (Plslbint bi | Plsrbint bi | Pasrbint bi), [u1;u2] ->
      let tbint = type_of_bigint bi in
      let tu1 = ulambda env tbint u1 in
      let tu2 = ulambda env (ref TTint) u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify tbint rt)

  | Pbintcomp (bi,_) , [u1;u2] ->
      let tbint = type_of_bigint bi in
      let tu1 = ulambda env tbint u1 in
      let tu2 = ulambda env tbint u2 in
	build_uterm (TypUprim (p,[tu1;tu2])) (sub_unify (ref TTbool) rt)

  | Pcvtbint (bi1,bi2) , [u] ->
      let tu = ulambda env (type_of_bigint bi1) u in
	build_uterm (TypUprim (p,[tu])) (sub_unify (type_of_bigint bi2) rt)
	  
  | p , ul -> 
      Printlambda.primitive Format.str_formatter p;
      failwith (Printf.sprintf "Rebuildtypes primitive failure for %s (%d arguments)" (Format.flush_str_formatter()) (List.length ul))


let freeze_id (id,typ) = (id,to_typeinfo typ)

let rec freeze_const = function
  | TypTConst_base c -> TypTConst_base c
  | TypTConst_pointer i -> TypTConst_pointer i
  | TypTConst_block(tag,scl) -> TypTConst_block(tag,List.map (fun (sc,typ) ->(freeze_const sc,to_typeinfo typ)) scl)
  | TypTConst_float_array sl -> TypTConst_float_array sl

let rec freeze tu = 
  let ftyp = to_typeinfo tu.utltype in
  let fterm = 
    match tu.utlterm with
	TypUvar id -> TypUvar id
      | TypUconst sc -> TypUconst (freeze_const sc)
      | TypUdirect_apply(lbl,l) -> TypUdirect_apply(lbl,List.map freeze l)
      | TypUgeneric_apply(t,tl) -> TypUgeneric_apply(freeze t,List.map freeze tl)
      | TypUclosure(funl,envl) -> TypUclosure(List.map (fun (lbl,ar,tids,body) -> (lbl,ar,List.map freeze_id tids,freeze body)) funl,
					      List.map freeze envl)
      | TypUoffset(ut,n) -> TypUoffset(freeze ut,n)
      | TypUlet(tid,t1,t2) -> TypUlet(freeze_id tid,freeze t1,freeze t2)
      | TypUletrec(decl,body) -> TypUletrec(List.map freeze_decl decl,freeze body)
      | TypUprim(p,tl) -> TypUprim(p,List.map freeze tl)
      | TypUswitch(arg,us) ->
	  TypUswitch(freeze arg,
		  {tus_index_consts=us.tus_index_consts;
		   tus_actions_consts=Array.map freeze us.tus_actions_consts;
		   tus_index_blocks=us.tus_index_blocks;
		   tus_actions_blocks=Array.map freeze us.tus_actions_blocks})
      | TypUstaticfail(n,utls) -> TypUstaticfail(n,List.map freeze utls)
      | TypUcatch(n,tids,t1,t2) -> TypUcatch(n,List.map freeze_id tids,freeze t1,freeze t2)
      | TypUtrywith(t1,tid,t2) -> TypUtrywith(freeze t1,freeze_id tid,freeze t2)
      | TypUifthenelse(t,t1,t2) -> TypUifthenelse(freeze t,freeze t1,freeze t2)
      | TypUsequence(t1,t2) -> TypUsequence(freeze t1,freeze t2)
      | TypUwhile(t1,t2) -> TypUwhile(freeze t1,freeze t2)
      | TypUfor(tid,t1,t2,dir,body) -> TypUfor(freeze_id tid,freeze t1,freeze t2,dir,freeze body)
      | TypUassign(tid,t) -> TypUassign(freeze_id tid,freeze t)
      | TypUsend(t1,t2,tl) -> TypUsend(freeze t1,freeze t2,List.map freeze tl) 
  in build_uterm fterm ftyp
and freeze_decl (tid,t) = (freeze_id tid,freeze t)

(* entry point *)

let retype_ulambda u =
  let rtu =
    let env = env_init() in 
      ulambda env (ref TTblock) u (* block is for the unit result of module expression *)
  in freeze rtu
