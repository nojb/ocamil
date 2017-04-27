(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ctypedlambda.ml,v 1.56 2007/03/24 06:30:00 montela Exp $ *)

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda
open Typedlambda
open Clambda
open Types
open Typemod
open Il


(* annotated Clambda *)

type 'a ti_structured_constant = 
  | TypTConst_base of constant (* Asttypes.constant already bear types *)
  | TypTConst_pointer of int
  | TypTConst_block of int * ('a ti_structured_constant * 'a) list
  | TypTConst_float_array of string list

type 'a utypedident = Ident.t * 'a
type 'a utypedlambda = {utlterm:'a utypedlambdaterm;utltype:'a}

and 'a utypedlambdaterm =
    TypUvar of Ident.t
  | TypUconst of 'a ti_structured_constant
  | TypUdirect_apply of function_label * 'a utypedlambda list
  | TypUgeneric_apply of 'a utypedlambda * 'a utypedlambda list
  | TypUclosure of (function_label * int * 'a utypedident list * 'a utypedlambda) list
              * 'a utypedlambda list
  | TypUoffset of 'a utypedlambda * int
  | TypUlet of 'a utypedident * 'a utypedlambda * 'a utypedlambda
  | TypUletrec of ('a utypedident * 'a utypedlambda) list * 'a utypedlambda
  | TypUprim of primitive * 'a utypedlambda list
  | TypUswitch of 'a utypedlambda * 'a utypedlambda_switch
  | TypUstaticfail of int * 'a utypedlambda list
  | TypUcatch of int * 'a utypedident list * 'a utypedlambda * 'a utypedlambda
  | TypUtrywith of 'a utypedlambda * 'a utypedident * 'a utypedlambda
  | TypUifthenelse of 'a utypedlambda * 'a utypedlambda * 'a utypedlambda
  | TypUsequence of 'a utypedlambda * 'a utypedlambda
  | TypUwhile of 'a utypedlambda * 'a utypedlambda
  | TypUfor of 'a utypedident * 'a utypedlambda * 'a utypedlambda * direction_flag * 'a utypedlambda
  | TypUassign of 'a utypedident * 'a utypedlambda
  | TypUsend of 'a utypedlambda * 'a utypedlambda * 'a utypedlambda list

and 'a utypedlambda_switch =
  { tus_index_consts: int array;
    tus_actions_consts: 'a utypedlambda array;
    tus_index_blocks: int array;
    tus_actions_blocks: 'a utypedlambda array}

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
 (*   mutable fun_inline: (utypedident list * utypedlambda) option  *)
}

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_integer of int
  | Value_constptr of int




(*********   builders **********)

let build_uterm term typ = {utlterm=term;utltype=typ}

let build_uvarterm tid = {utlterm=TypUvar (fst tid);utltype=snd tid}

(* also used to propagate back type informations downto the leaves *)
(* useful for parts of code generated during pattern matching compilation *)

let rec build_uterm_back term typ =
  match term with
      TypUlet (tid,u1,u2) -> build_uterm (TypUlet(tid,u1,build_uterm_back u2.utlterm typ)) typ
    | TypUcatch (i,ids,u1,u2) -> build_uterm (TypUcatch (i,ids,build_uterm_back u1.utlterm typ,build_uterm_back u2.utlterm typ)) typ
    | TypUifthenelse (u,u1,u2) -> build_uterm (TypUifthenelse (u,build_uterm_back u1.utlterm typ,build_uterm_back u2.utlterm typ)) typ
    | TypUswitch(u,sw) -> 
	let sw2 = {sw with tus_actions_consts = Array.map (fun x -> build_uterm_back x.utlterm typ) sw.tus_actions_consts;
		     tus_actions_blocks = Array.map (fun x -> build_uterm_back x.utlterm typ) sw.tus_actions_blocks}
	in build_uterm (TypUswitch(u,sw2)) typ
    | TypUsequence(u1,u2) -> build_uterm (TypUsequence(u1,build_uterm_back u2.utlterm typ)) typ
    | _ -> build_uterm term typ

let build_usequenceterm (tl1,tl2) typ =
  let rt,tl2' = 
    if typ = TIdontknow then tl2.utltype, tl2
    else typ, build_uterm_back tl2.utlterm typ 
  in build_uterm (TypUsequence(tl1,tl2')) rt

let build_uletterm (tid,tl1,tl2) typ =
  (* TODO cleanup: use id instead of tid (type duplication ...) *)
  let rt,tl2' = 
    if typ = TIdontknow then tl2.utltype, tl2
    else typ, build_uterm_back tl2.utlterm typ 
  in build_uterm (TypUlet(tid,tl1,tl2')) rt

let build_ucatchterm (i,ids,tl1,tl2) typ =
   let rt,tl1',tl2' = match typ with 
       TIdontknow -> begin match tl1.utltype,tl2.utltype with
	   (TIdontknow,ta) -> ta,build_uterm_back tl1.utlterm ta,tl2
	 | (ta,_) -> ta,tl1,build_uterm_back tl2.utlterm ta end
     | _ -> typ,build_uterm_back tl1.utlterm typ,build_uterm_back tl2.utlterm typ
   in build_uterm (TypUcatch(i,ids,tl1',tl2')) rt

let build_uifthenelseterm (tl,tl1,tl2) typ =
  let rt,tl1',tl2' = match typ with 
      TIdontknow -> begin match tl1.utltype,tl2.utltype with
	  (TIdontknow,ta) -> ta,build_uterm_back tl1.utlterm ta,tl2
	| (ta,_) -> ta,tl1,build_uterm_back tl2.utlterm ta end
    | _ -> typ,build_uterm_back tl1.utlterm typ,build_uterm_back tl2.utlterm typ
  in let tl' = build_uterm tl.utlterm TIbool
  in build_uterm (TypUifthenelse(tl',tl1',tl2')) rt

let build_uswitchterm (tl,sw) typ =
  let rec extract_type = function
      [] -> TIdontknow
    | {utltype=TIdontknow}::rem -> extract_type rem 
    | {utltype=type_ann}::rem -> type_ann
  in
  let rt = match typ with 
      TIdontknow -> extract_type ((Array.to_list sw.tus_actions_consts) @ (Array.to_list sw.tus_actions_blocks) ) 
    | _ -> typ
  in
  let update_rt ul = if ul.utltype = rt then ul else build_uterm_back ul.utlterm rt in
  let sw2 = {sw with tus_actions_consts = Array.map update_rt sw.tus_actions_consts;
	       tus_actions_blocks = Array.map update_rt sw.tus_actions_blocks} in
    build_uterm (TypUswitch(tl,sw2)) rt



let arrow_apply ti nb =
  let rec aux args res n =
    match args with
	[] -> if n = 0 then res else failwith (Printf.sprintf "Ctypedlambda.arrow_apply(X) %d/%s" nb (typeinfo_to_string ti))
      | t::q -> 
	  if n = 0 then TIarrow(args,res)
	  else aux q res (n-1) 
  in
    match ti with
	(* nb < 0 in case of uncurrified function: all parameters are grouped in a single tuple type *)
	  TIarrow(args,res) -> if nb > 0 then aux args res nb else aux args res 1
      | _ -> failwith ("Ctypedlambda.arrow_apply " ^ (typeinfo_to_string ti))
	  

let build_uprimterm  (p,args) t =
  if t = TIdontknow then begin
    let type_of_bint = function
	Pnativeint -> TInint
      | Pint32 -> TIint32
      | Pint64 -> TIint64
    in
    let typ =
      match p,args with
	  Pidentity,[arg] -> arg.utltype
	| Pignore, _ -> TIvoid
	| Pgetglobal gid, _ -> TIdontknow
	| Psetglobal gid, _ -> TIvoid
	| Pmakeblock (tag,_),_ -> TIdontknow
	| Pfield i,_  -> TIdontknow
	| Pfldtag (i,tag),_  -> TIdontknow
	| Psetfield _,_ -> TIvoid
	| Pfloatfield  _,_ -> TIfloat
	| Psetfloatfield  _,_ -> TIvoid
	| Pccall desc, _ -> TIdontknow
	| Praise ,_ -> TIdontknow
	| (Psequand | Psequor | Pnot
	  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
	  | Pandint | Porint | Pxorint
	  | Plslint | Plsrint | Pasrint),_ -> TIint
	| Pintcomp _,_ -> TIbool
	| Poffsetint  _,_ -> TIint
	| Poffsetref  _,_ -> TIvoid
	| Pintoffloat , _ -> TIint
	| Pfloatofint, _ -> TIfloat
	| (Pnegfloat | Pabsfloat
	  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat), _ -> TIfloat
	| Pfloatcomp  _,_ -> TIbool
	| Pstringlength, _ -> TIstring
	| (Pstringrefu | Pstringrefs) , _ -> TIchar
	| (Pstringsetu | Pstringsets) , _ -> TIvoid
	| Pmakearray kind, _ -> TIarray TIobject
	| Parraylength kind,_ -> TIint
	| Parrayrefu kind,_ -> TIobject
	| Parraysetu kind,_ -> TIvoid
	| Parrayrefs kind,_ -> TIobject
	| Parraysets kind,_ -> TIvoid
	| (Pisint| Pisout | Pbittest), _ -> TIbool
	| Pbintofint bi, _ -> type_of_bint bi
	| Pintofbint _,_ -> TIint
	| Pcvtbint (_,bi),_ -> type_of_bint bi
	| (Pnegbint bi | Paddbint bi | Psubbint bi | Pmulbint bi | Pdivbint bi | Pmodbint bi
	  | Pandbint bi | Porbint bi | Pxorbint bi | Plslbint bi | Plsrbint bi | Pasrbint bi),_ -> type_of_bint bi
	| Pbintcomp _,_ -> TIbool
	| Pbigarrayref _,_ -> failwith "BIG array (1)"
	| Pbigarrayset _,_ -> failwith "BIG array (2)"
	| Pil str,_ -> TIdontknow
	| _ -> failwith "Ctypedlambda.build_uprimterm"
    in
      build_uterm (TypUprim(p,args)) typ
  end
  else build_uterm (TypUprim(p,args)) t

(* to forget type annotations *)

let rec to_ulambda_const = function
  | TypTConst_base c -> Const_base c
  | TypTConst_pointer i -> Const_pointer i
  | TypTConst_block(tag,scl) -> Const_block(tag,List.map (fun (sc,_) -> to_ulambda_const sc) scl)
  | TypTConst_float_array sl -> Const_float_array sl

let rec to_ulambda utl = match utl.utlterm with
    TypUvar id -> Uvar id
  | TypUconst sc -> Uconst (to_ulambda_const sc)
  | TypUdirect_apply(lbl,l) -> Udirect_apply(lbl,List.map to_ulambda l)
  | TypUgeneric_apply(t,tl) -> Ugeneric_apply(to_ulambda t,List.map to_ulambda tl)
  | TypUclosure(funl,envl) -> Uclosure(List.map (fun (lbl,ar,tids,body) -> 
						   lbl.funtype <- TInotimplemented "Type erased";
						   (lbl,ar,List.map fst tids,to_ulambda body)) funl,
				       List.map to_ulambda envl)
  | TypUoffset(ut,n) -> Uoffset(to_ulambda ut,n)
  | TypUlet((id,_),t1,t2) -> Ulet(id,to_ulambda t1,to_ulambda t2)
  | TypUletrec(decl,body) -> Uletrec(List.map to_ulambda_decl decl,to_ulambda body)
  | TypUprim(p,tl) -> Uprim(p,List.map to_ulambda tl)
  | TypUswitch(arg,us) ->
      Uswitch(to_ulambda arg,
	      {us_index_consts=us.tus_index_consts;
	       us_actions_consts=Array.map to_ulambda us.tus_actions_consts;
	       us_index_blocks=us.tus_index_blocks;
	       us_actions_blocks=Array.map to_ulambda us.tus_actions_blocks})
  | TypUstaticfail(n,utls) -> Ustaticfail(n,List.map to_ulambda utls)
  | TypUcatch(n,tids,t1,t2) -> Ucatch(n,List.map fst tids,to_ulambda t1,to_ulambda t2)
  | TypUtrywith(t1,(id,_),t2) -> Utrywith(to_ulambda t1,id,to_ulambda t2)
  | TypUifthenelse(t,t1,t2) -> Uifthenelse(to_ulambda t,to_ulambda t1,to_ulambda t2)
  | TypUsequence(t1,t2) -> Usequence(to_ulambda t1,to_ulambda t2)
  | TypUwhile(t1,t2) -> Uwhile(to_ulambda t1,to_ulambda t2)
  | TypUfor((id,_),t1,t2,dir,body) -> Ufor(id,to_ulambda t1,to_ulambda t2,dir,to_ulambda body)
  | TypUassign((id,_),t) -> Uassign(id,to_ulambda t)
  | TypUsend(t1,t2,tl) -> Usend(to_ulambda t1,to_ulambda t2,List.map to_ulambda tl)
and to_ulambda_decl (tid,t) = (fst tid,to_ulambda t)


(* type handling *)


module NamedTypePath = 
  (struct
     type t = namedtype
     let equal ntp1 ntp2 = ntp1.nt_ext = ntp2.nt_ext && ntp1.nt_path = ntp2.nt_path
     let hash ntp = Hashtbl.hash ntp.nt_path
   end:Hashtbl.HashedType with type t = namedtype)


module HashedPath = Hashtbl.Make(NamedTypePath)

type namedtype_info = 
    NTIrecord of (string * typeinfo) list
  | NTIvariant of (string * typeinfo list) list

(* table describing algebraic types encountered so far *)
let algebtypes_table = (HashedPath.create 20 : namedtype_info HashedPath.t)

let get_record_description apa =
  match HashedPath.find algebtypes_table apa with
    | NTIrecord desc -> desc
    | _ -> failwith "get_record_description"

let get_variant_description apa =
  match HashedPath.find algebtypes_table apa with
    | NTIvariant desc -> desc
    | _ -> failwith "get_variant_description"


let list_id = {nt_ext=true;nt_path=["$list"]} 
let list_cstrs = [("Nil",[]);("Cons",[TIobject;TIvariant list_id])]

let option_id = {nt_ext=true;nt_path=["$option"]}
let option_cstrs = [("None",[]);("Some",[TIobject])]

let init_algebtypes_table () = 
  HashedPath.clear algebtypes_table;
  HashedPath.add algebtypes_table list_id (NTIvariant list_cstrs);
  HashedPath.add algebtypes_table option_id (NTIvariant option_cstrs)


let rec display_typedecls ctd = 
  Printf.printf "%s [" ctd.ctd_name;
  List.iter (fun (nme,_,_) -> Printf.printf "%s;" nme) ctd.ctd_decls;
  Printf.printf "\n";
  List.iter display_typedecls ctd.ctd_children;
  Printf.printf "]\n"

let remove_functors_instances modname = 
  try
    let l = String.index modname '(' 
    and r = String.rindex modname ')' in
      (String.sub modname 0 l) ^ (String.sub modname (r+1) ((String.length modname)-r-1))
  with Not_found -> modname

(* Caution : paths are not "absolute" (ie expressed from the top of the implementation file) *)
(* TODO: homonyms could be worked out when type descriptions are different *)
let absolute_path moduleimmersion pa = (* mi is path of typeref location, pa is path given by the typeref *)
  let current_module = !Utils.current_unit_name() in
  let dpath = List.map remove_functors_instances (Utils.decompose_pathname (Path.name pa)) in
  let typname = List.hd dpath in
  let revdpath = List.rev dpath in
  let dpath = if (List.hd revdpath) = current_module then List.rev (List.tl revdpath) else dpath in
  let givenpath = List.rev (List.tl dpath) in
  let rec is_correct_path ctd  = function (* true when pa is found in ctd local typedefs *)
	[] -> List.exists (fun (s,_,_) -> s=typname) ctd.ctd_decls
      | pathhead::innerpath -> 
	  try 
	    let ctd2 = List.find (fun ctd->ctd.ctd_name=pathhead) ctd.ctd_children in 
	      is_correct_path ctd2 innerpath
	  with Not_found -> false
  in
  let rec search mdl ctd accu = (* returns absolute path, which is accu (module-digged path) + dpath *)
(* during search, we dig in ctd typedecls (making accu grow) using prefixes mdl of typeref location *)
    if is_correct_path ctd givenpath then dpath@accu
    else
      match mdl with
	  [] -> raise Not_found
	| md::rem -> 
	    let ctd2 = List.find (fun ctd->ctd.ctd_name=md) ctd.ctd_children in (* may raise Not_found *)
	      search rem ctd2 (md::accu)
  in
  let typedecls = if !Clflags.toplevel_mode then Typemod.updated_toplevel_typedecls() else Typemod.camil_typedecls in
    if !Clflags.verbose then display_typedecls typedecls;
  let realpath, is_ext = 
    if Path.binding_time pa != 0 then begin
	try (search (List.rev moduleimmersion) typedecls [], false) 
	with Not_found -> 
	  let s = ref "" and s' = ref "" in 
	  let _ = List.iter (fun x -> s:= !s ^ x ^ ";") moduleimmersion in
	  let _ = List.iter (fun x -> s':= x ^ "." ^ !s') dpath in
	  let _ = Printf.sprintf "Ctypedlambda.absolute_path: cannot find %s (aka %s) internally from %s: assumed to be external." (Path.name pa) !s' !s in
	    dpath, true
      end 
    else dpath, true
  in
    {nt_ext = is_ext ; nt_path = realpath}

(* failed attempt based on environnements, kept for reference *)
(* let absolute_path moduleimmersion env pa =
  let is_correct_path qname =
    Printf.printf ">> Looking at %s\n" qname;flush stdout;
    try 
      let rootpath,def = Env.lookup_type (Longident.parse qname) env in
	Printf.printf ">> Found rootpath %s\n" (Path.name rootpath);
	(* assumption : all references to a defined type share a unique type_declaration value *)
	def == Env.find_type pa env
    with Not_found -> Printf.printf ">> (Not found)\n";false
  in
  let rec search mdl guesspath =
    if guesspath = "" && is_correct_path (Path.name pa) then "" 
    else
      match mdl with 
	  [] -> raise Not_found
	| md::rem -> 
	    let nextguess = if guesspath = "" then md else guesspath^"."^md in
	      if is_correct_path (nextguess^"."^(Path.name pa)) then nextguess
	      else search rem nextguess
  in

    if Path.binding_time pa != 0 then begin
      let soluce = try (search (List.rev moduleimmersion) "")^(Path.name pa) with Not_found -> "%Not_found%" in 
	Printf.printf "path[%s];imm[%s]=>%s\n" 
	  (Path.name pa) 
	  (List.fold_left (fun s md -> if s="" then md else md^"."^s) ""  moduleimmersion)
	  soluce;
	flush stdout
    end;
     {nt_ext=Path.binding_time pa = 0 ; nt_path = (Path.name pa)::moduleimmersion}
*)


(* categories of builtin types : see Predef.build_initial_env *)
let rec convert_typeannotation shallregister env moduleimmersion te =
  let te = Ctype.full_expand env te in
    match te.desc with
	Tvar | Tunivar -> TIobject
	| Tarrow (_,t1,t2,_) ->
	    let t1' = convert_typeannotation  shallregister env moduleimmersion t1 in
	    let t2' = convert_typeannotation  shallregister env moduleimmersion t2 in
	      begin
		match t2' with
		| TIarrow(args,res) -> TIarrow(t1'::args,res)
		| _ -> TIarrow([t1'],t2')
	      end
	| Ttuple tel -> TItuple (List.map (convert_typeannotation  shallregister env moduleimmersion) tel)
	| Tconstr (pa,tel,_) -> begin  try
	    let tdecl = Env.find_type pa env in
	      match tdecl.type_kind with
		  Type_abstract -> begin 
		    match Path.name pa with 
			"int" -> TIint
		      | "int32" -> TIint32
		      | "int64" -> TIint64
		      | "nativeint" -> TInint
		      | "float" -> TIfloat
		      | "char" -> TIchar
		      | "string" -> TIstring
		      | "array" -> TIarray (convert_typeannotation  shallregister env moduleimmersion (List.hd tdecl.type_params))
		      | "format" -> TIstring
		      | "lazy_t" -> TIlazy (convert_typeannotation  shallregister env moduleimmersion (List.hd tdecl.type_params))
		      | x -> TIobject
		  end
		| Type_variant cstrs -> begin
		    match Path.name pa with
		      | "bool" -> TIbool
		      | "unit" -> TIunit
		      | "exn" -> TIexception
		      | "list" -> TIlist (convert_typeannotation  shallregister env moduleimmersion (List.hd tdecl.type_params))
		      | "option" -> TIoption (convert_typeannotation  shallregister env moduleimmersion (List.hd tdecl.type_params))
		      | _ -> 
			  let apa = absolute_path moduleimmersion pa in
			    if shallregister then register_variant env moduleimmersion apa cstrs;
			    TIvariant apa
		  end
		| Type_record (flds,repr) -> 
		    let apa = absolute_path moduleimmersion pa in
		      if shallregister then register_recordtype env moduleimmersion apa flds;
		      TIrecord apa
	  
	      with Not_found -> TIobject
	  end
	| Tvariant _ -> TIblock
	| Types.Tobject (_,_) -> TIblock
	| Tfield (_,_,_,_) -> failwith "Ctypedlambda.convert_typeannotation  [Tfield]"
	| Tnil -> failwith "Ctypedlambda.convert_typeannotation  [Tnil]"
	| Tlink _  -> failwith "Ctypedlambda.convert_typeannotation  [Tlink]"
	| Tsubst _  -> failwith "Ctypedlambda.convert_typeannotation  [Tsubst]"
	| Tpoly (t1,_) -> convert_typeannotation shallregister env moduleimmersion t1

and register_recordtype env moduleimmersion apa flds =
  if not (HashedPath.mem algebtypes_table apa) then begin
    if !Clflags.verbose then Printf.printf "Add record reference: %s\n" (print_namedtype_path apa);
    HashedPath.add algebtypes_table apa (NTIrecord (List.map (fun (s,_,texp) -> (s,convert_typeannotation  false env moduleimmersion texp)) flds));
    List.iter (fun (s,_,texp) -> ignore (convert_typeannotation  true env moduleimmersion texp)) flds
  end
and register_variant env moduleimmersion apa cstrs =
  if not (HashedPath.mem algebtypes_table apa) then begin
    if !Clflags.verbose then Printf.printf "Add variant reference: %s\n" (print_namedtype_path apa);
    HashedPath.add algebtypes_table apa (NTIvariant (List.map (fun (s,tel) -> (s,List.map (convert_typeannotation  false env moduleimmersion) tel)) cstrs));
    List.iter (fun (s,tel) -> ignore (List.map (convert_typeannotation  true env moduleimmersion) tel)) cstrs
  end

(* transformation function : ML type annotation -> accurate_typeinfo *)
let get_accurate_typeinfo tann =
  try 
    match tann with 
	None -> TIdontknow
      | Some {taexpr=te;taenv=env;tapath=pa} -> convert_typeannotation true env pa te
    with x -> Printtypedlambda.type_expr_option Format.std_formatter tann;flush stdout;raise x

let rec const_accurate_typeinfo cst = match cst with
    TConst_base c -> TypTConst_base c
  | TConst_pointer i -> TypTConst_pointer i
  | TConst_block(tag,scl) -> TypTConst_block(tag,List.map (fun (sc,tann)->(const_accurate_typeinfo sc,get_accurate_typeinfo tann)) scl)
  | TConst_float_array sl -> TypTConst_float_array sl
