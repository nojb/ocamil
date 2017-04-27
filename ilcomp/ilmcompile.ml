(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ilmcompile.ml,v 1.6 2007/03/19 15:48:19 montela Exp $ *)

open Asttypes
open Primitive
open Lambda
open Typedlambda
open Clambda 
open Ctypedlambda
open Ilm

open Compilenv
open Il
 
let bintkind_to_type = function 	
  | Pint32 -> CTint32
  | Pint64 -> CTint64
  | Pnativeint -> CTnint


let get_primitive_signature prim ctl =
  let argt,rt =
    match prim with 
	CPnewprim p -> begin match p,ctl with
	  | TP_get_builtin _ , [] -> [] , CTobject
	  | TP_get_global _ , []-> [] , (CTarray CTobject)
	    
	  | TP_get_field {dtype=fct} , [cct] -> [cct] , fct
	  | TP_set_field {dtype=fct} , [cct] -> [cct;fct] , CTvoid
	      
	  | (TP_mktop _ | TP_mkenv _  | TP_mkrec _ | TP_mkclos _) , _ -> assert false (* PPPP assigned in compimp *)
	      
	  | TP_buildobject _ , rt::argt -> argt , rt
	      
	  | TP_raise ct , []-> [CTarray CTobject] , ct
	  | TP_eq , [ct] -> [ct;ct],CTbool
	  | TP_neq , [ct] -> [ct;ct],CTbool
	  | TP_cast(ct1,ct2) , [] -> [ct1],ct2
	  | TP_pushdummy ct , [] -> [],ct
	  | TP_pushint i , [] -> [],CTint
	      (*  | TP_bittest of string *)
	  | TP_convint bi , [ct] -> [ct], (bintkind_to_type bi)
	  | TP_offsetref _ , [] -> [CTrecord {nt_ext=not !Clflags.nopervasives;nt_path=["ref";"Pervasives"]}],CTvoid (* for records as classes ! *) 

	  | TP_set_block _ , [] -> [CTarray CTobject;CTobject],CTvoid
	  | TP_get_block _ , [] -> [CTarray CTobject],CTobject
	  | TP_string_length ct , [] -> [ct] , CTint
	  | TP_string_set(ct,_) , [] -> [ct;CTint;CTchar] , CTvoid
	  | TP_string_ref(ct,_) , [] -> [ct;CTint] , CTchar
	  | TP_stringcomp _ , [] -> [CTstring;CTstring] , CTbool

	  | _ , _ -> assert false
	end
      | CPlegacyprim p -> begin match p,ctl with
	  | p , [] when List.mem p 
	      [ Paddint ; Psubint ; Pmulint ; Pdivint ; Pmodint ; Pandint ; 
		Porint ; Pxorint ; Plslint ; Plsrint ; Pasrint ] -> [CTint;CTint],CTint
	  | Pnegint , [] -> [CTint],CTint
	  | Pintcomp _ , [ct] -> [ct;ct],CTbool  
	  | Poffsetint _ , [] -> [CTint],CTint
	  | Poffsetref _ , [] -> [CTarray CTobject],CTvoid
	  | (Psequor | Psequand) , [] -> [CTbool;CTbool] , CTbool 
	  | Pnot , [] -> [CTbool] , CTbool
	  | p , [] when List.mem p [Paddfloat ; Psubfloat ; Pmulfloat ; Pdivfloat] -> [CTfloat;CTfloat] , CTfloat
	  | (Pabsfloat | Pnegfloat) , [] -> [CTfloat] , CTfloat
	  | Pfloatcomp _ , [] -> [CTfloat;CTfloat] , CTbool  
	  | Pintoffloat , [] -> [CTfloat] , CTint
	  | Pfloatofint , [] -> [CTint] , CTfloat

	  | Pmakeblock _ , ctl -> List.map (fun ct -> CTobject) ctl , (CTarray CTobject)

	  | Pmakearray _ , ctl -> List.map (fun ct -> CTobject) ctl , (CTarray CTobject)
	  | Parraylength _ , [] -> [CTarray CTobject] , CTint
	  | (Parrayrefu _ | Parrayrefs _) , [] -> [CTarray CTobject;CTint] , CTobject
	  | (Parraysetu _ | Parraysets _) , [] -> [CTarray CTobject;CTint;CTobject] , CTvoid

	  | Pccall _ , rt::argt -> argt , rt
	  | Pisint , [] -> [CTobject], CTbool (* not used anymore*)
	  | Pisout , [] -> [CTint;CTint] , CTbool

	  | (Paddbint bi | Psubbint bi | Pmulbint bi | Pdivbint bi | Pmodbint bi
	    | Pandbint bi | Porbint bi | Pxorbint bi) , [] -> let ct = bintkind_to_type bi in [ct;ct] , ct
	  | Pnegbint bi , [] ->  let ct = bintkind_to_type bi in [ct] , ct
	  | (Plslbint bi | Plsrbint bi | Pasrbint bi) , [] -> let ct = bintkind_to_type bi in [ct;CTint] , ct
	  | Pbintcomp(bi,_) , [] ->  let ct = bintkind_to_type bi in [ct;ct] , CTbool

	  | Pidentity , [ct] -> [ct] , ct


	  | _ , _ -> 
	      Printlambda.primitive Format.str_formatter p;
	      failwith ("Primitive CT signature for "^(Format.flush_str_formatter()))
	end
  in
    prim,argt,rt
	
(* data representation *)
(* TODO: merge with similar code in Compil *)
(* for cst ctr optim *)
open Ilpredef;;
let empty_ns = ""

let global_const_ctr_field i = 
  let global_const_ctr_ref = Ilbuild.type_ref empty_ns "MLTop" in
  let asb = if !Clflags.compiling_camil_corelib then ThisAssembly
  else Ilpath.build_extscope Naming.core_camil_assbly in
    global_const_ctr_ref.trscp <- asb;
    let const_ctr_type = 
      if not !Clflags.rebuiltmode && not !Clflags.variantrepr_objarray then Ilpredef.variant_type
      else objtab_type in
      Ilbuild.field_ref global_const_ctr_ref 
	("const_ctr"^(string_of_int i)) const_ctr_type
	(** **)


let rec downgrade_typeinfo ti = 
  match ti with
    | TIint | TIchar | TIint32 | TIint64 | TInint |  TIbool 
    | TIunit  | TIvoid
    | TIfloat | TIstring
    | TIgenclosure | TIsharedclosure 
    | TIobject | TIpureIL _
    | TIdontknow | TInotimplemented _ -> ti
    | TIarrow (til,ti2) -> TIarrow(List.map downgrade_typeinfo til,downgrade_typeinfo ti2)
    | _ -> TIblock


let funlabel_typeinfo ti = 
  if !Clflags.rebuiltmode then downgrade_typeinfo ti 
  else ti

let variant_specialization ti tag =
  if !Clflags.rebuiltmode || !Clflags.variantrepr_objarray then assert false
  else match ti with TIvariant tid ->
    if Ilm.is_immediate_typeinfo ti then CTint else CTvariant (tid,Some tag)
    | _ -> assert false

let should_specialize_variant u =
  if !Clflags.rebuiltmode || !Clflags.variantrepr_objarray then None
  else match u.utlterm,u.utltype with 
      TypUvar id, TIvariant pa -> Some (id,u.utltype)
    | _ -> None

let typeinfo_to_il tinfo = Ilm.to_il (Ilm.ctstype_of_typeinfo tinfo)


let build_record_prim id =
  let cid = record_class_server id in
  let descr = get_record_description id in
  let fullctorsig = List.map (fun (fnme,tinfo) -> (typeinfo_to_il tinfo,Some fnme)) descr in
  let flds = List.map (fun (fnme,tinfo) -> Ilbuild.field_ref cid fnme (typeinfo_to_il tinfo)) descr in
    get_primitive_signature (CPnewprim (TP_buildobject (cid,fullctorsig,flds)))
      ((CTrecord id)::(List.map (fun (_,ti) -> Ilm.ctstype_of_typeinfo ti) descr))

let read_record_prim id nb =
  let name,ti = List.nth (get_record_description id) nb in
  let ctyp = Ilm.ctstype_of_typeinfo ti in
    get_primitive_signature (CPnewprim(TP_get_field {did = (record_class_server id , name);dtype = ctyp})) [CTrecord id]

let write_record_prim id nb =
  let name,ti = List.nth (get_record_description id) nb in
  let ctyp = Ilm.ctstype_of_typeinfo ti in
    get_primitive_signature (CPnewprim(TP_set_field {did = (record_class_server id , name);dtype = ctyp})) [CTrecord id]
      
let build_variant_prim id tag cstrs typeinfo =
  let cname,descr = List.nth cstrs tag in
    if (List.length descr)=0 && not !Clflags.noctropt && not !Clflags.rebuiltmode && not !Clflags.variantrepr_objarray
    then begin (* cst ctr optim *) 
      if tag >= 256 then Utils.bug "Ilmcompile.variant optim" ">=256 !!";
      if Ilm.is_immediate_typeinfo typeinfo then
	get_primitive_signature (CPnewprim (TP_pushint tag)) []
      else
	let ct = Ilm.ctstype_of_typeinfo typeinfo in
	let fref = global_const_ctr_field tag in
	let tfld = {did = (fref.frcla,fref.frnme) ; dtype = ct} in
	  (CPnewprim (TP_get_field tfld)),[],ct 
    end
    else 
      let cid = variant_class_server id (Some tag) in
      let fullctorsig = List.map (fun tinfo -> (typeinfo_to_il tinfo,None)) descr in (* variant constructors don't use explicit args names *)
      let rec build_fdec accu idx = function
	  [] -> List.rev accu
	| tinfo::rem -> 
	    build_fdec ((Ilbuild.field_ref cid ("x"^(string_of_int idx)) (typeinfo_to_il tinfo))::accu) (idx+1) rem in
      let flds = build_fdec [] 0 descr in 
	get_primitive_signature (CPnewprim (TP_buildobject(cid,fullctorsig,flds)))
				   ((CTvariant (id,Some tag))::(List.map (fun ti -> Ilm.ctstype_of_typeinfo ti) descr))
				   

let read_variant_prim id tag nb cstrs =
  let cname,cargs = List.nth cstrs tag in
  let ti = 
    try List.nth cargs nb 
    with Failure "nth" -> failwith (Printf.sprintf "Ilmcompile.block_field_access:nth %s %d" cname nb) in
  let ctyp = Ilm.ctstype_of_typeinfo ti in
  let fieldref = variant_class_server id (Some tag), "x"^(string_of_int nb) in
    get_primitive_signature (CPnewprim (TP_get_field {did = fieldref ; dtype = ctyp})) [CTvariant (id,Some tag)]
      

(* find a unit name in .cmx information *)

let find_unit id = (* only used fot get_global primitive *)
  let cui=Compilenv.get_current_unit () in 
  let modname = Ident.name id in 
  (* REM: second case used for toplevel *)
  if (modname=cui.ui_name || modname="$specialuse_toplevel") 
  then Naming.get_unit_id ()
  else let (_,y,_) = 
     if (!Clflags.verbose) then (print_string "----------"; print_newline());
     List.find (fun (x,_,_) -> 
		  if (!Clflags.verbose) then (print_string x; print_string "<-->"; 
                               print_string modname; print_string "|"; print_newline());
		  x=modname) 
       (List.rev cui.ui_imports_cmx) (* temp patch for toplvel *) in
    y.trnsp


let funCTobject x = CTobject

let rec select_primitive ct rt dwncstids (p,ul) = match (p,ul) with
  (* blocks *) 

  | Pmakeblock (tag,_) , ul  -> begin
      match rt with
	| TIblock | TItuple _ | TIlazy _ | TIexception -> get_primitive_signature (CPlegacyprim p) (List.map funCTobject ul)
	| TIrecord _ when !Clflags.recordrepr_objarray -> get_primitive_signature (CPlegacyprim p) (List.map funCTobject ul)
	| (TIvariant _ | TIlist _ | TIoption _) when !Clflags.variantrepr_objarray -> get_primitive_signature (CPlegacyprim p) (List.map funCTobject ul)

	| TIrecord id -> build_record_prim id 
	| TIvariant id -> build_variant_prim id tag (get_variant_description id) rt
	| TIlist _ -> build_variant_prim list_id tag list_cstrs rt
	| TIoption _ -> build_variant_prim option_id tag option_cstrs rt

	| TIarray TIobject (* still used for modules/functors *) ->  get_primitive_signature (CPlegacyprim p) (List.map funCTobject ul)
	| _ -> failwith ("Ilmcompile.makeblock : "^Clambda.typeinfo_to_string rt)
    end


  | (Pfield n | Pfloatfield n) , [u] -> begin
      match u.utltype with
	| TIblock | TItuple _ | TIlazy _ | TIexception | TIarray TIobject (* fields of globals are typed as obj[] *) -> 
	    get_primitive_signature (CPnewprim (TP_get_block n)) []
	| TIrecord _ when !Clflags.recordrepr_objarray -> get_primitive_signature (CPnewprim (TP_get_block n)) []
	| TIrecord id -> read_record_prim id n 
	    (* fallback for rebuilt types *)
	| _ when !Clflags.rebuiltmode -> get_primitive_signature (CPnewprim (TP_get_block n)) [] 
	| _ -> failwith ("Ilmcompile.field["^(string_of_int n)^"]: "^Clambda.typeinfo_to_string u.utltype)
    end

  | Pfldtag (n,tag) , [u] ->
      begin
	match u.utlterm with
	    TypUvar id -> begin try 
	      let (_,_,bref) = List.find (fun (id',_,_) -> id = id') dwncstids in
		bref := true
	    with Not_found -> ()
	    end
	  | _ -> ()
      end;
      begin match u.utltype with      
	| TIblock | TItuple _ | TIlazy _ | TIexception  -> get_primitive_signature (CPnewprim (TP_get_block n)) []
	| TIrecord _ when !Clflags.recordrepr_objarray  -> get_primitive_signature (CPnewprim (TP_get_block n)) []
	| (TIvariant _ | TIlist _ | TIoption _) when !Clflags.variantrepr_objarray -> get_primitive_signature (CPnewprim (TP_get_block n)) []
	| TIvariant id -> read_variant_prim id tag n (get_variant_description id)
	| TIlist _ -> read_variant_prim list_id tag n list_cstrs
	| TIoption _ -> read_variant_prim option_id tag n option_cstrs
	| _ when !Clflags.rebuiltmode -> get_primitive_signature (CPnewprim (TP_get_block n)) [] (* rebuilt types fallback *)
	| _ -> failwith ("Ilmcompile.fldtag : "^Clambda.typeinfo_to_string u.utltype)
      end

  | (Psetfield (n,_) | Psetfloatfield n) , [u1;u2] -> begin
      match u1.utltype with
	| TIblock | TIarray TIobject (* fields of globals are typed as obj[] *) -> 
	    get_primitive_signature (CPnewprim (TP_set_block n)) []
	| TIrecord _ when !Clflags.recordrepr_objarray -> get_primitive_signature (CPnewprim (TP_set_block n)) []
	| TIrecord id -> write_record_prim id n
	| _ -> failwith ("Ilmcompile.setfield : "^Clambda.typeinfo_to_string u1.utltype)
    end

(* arrays *)
  | Pmakearray _ , ul -> 
      begin 
	match rt with
	  | TIblock -> get_primitive_signature (CPlegacyprim (Pmakeblock(0,Asttypes.Mutable))) (List.map funCTobject ul)
	  | TIrecord _ when !Clflags.recordrepr_objarray -> (* homogeneic records may be seen as arrays *)
	      get_primitive_signature (CPlegacyprim (Pmakeblock(0,Asttypes.Mutable))) (List.map funCTobject ul)
	  | TIarray _  -> get_primitive_signature (CPlegacyprim p) (List.map funCTobject ul)
	  | TIrecord id when not !Clflags.recordrepr_objarray ->  (* homogeneic records may be seen as arrays *)
	      build_record_prim id
	  | _ ->  failwith ("Ilmcompile.makearray : "^Clambda.typeinfo_to_string rt)
      end

  | (Parrayrefu _ | Parrayrefs _) , [u1;u2] -> 
      begin 
	match u1.utltype with
	    | TIblock -> get_primitive_signature (CPlegacyprim p) []
	    | TIrecord _ when !Clflags.recordrepr_objarray -> (* homogeneic records may be seen as arrays *)
		get_primitive_signature (CPlegacyprim p) []
	    | TIarray _ -> get_primitive_signature (CPlegacyprim p) []
	    | TIrecord id when not !Clflags.recordrepr_objarray ->  (* homogeneic records may be seen as arrays *)
		begin match u2.utlterm with 
		    TypUconst (TypTConst_base (Const_int nb)) -> read_record_prim id nb
		  | _ -> failwith ("Ilmcompile.Parrayref(2) : "^Clambda.typeinfo_to_string u1.utltype)
		end
	    | TIobject -> get_primitive_signature (CPlegacyprim p) [] (* WARNING!! for Obj.magic cases in rebuildmode (cf boxed_identity) *)
	    | _ ->  failwith ("Ilmcompile.Parrayref : "^Clambda.typeinfo_to_string u1.utltype)
      end

 | (Parraysetu _ | Parraysets _) , [u1;u2;u3] -> 
      begin 
	match u1.utltype with
	  | TIblock ->  get_primitive_signature (CPlegacyprim p) []
	  | TIrecord _ when !Clflags.recordrepr_objarray -> (* homogeneic records may be seen as arrays *)
	      get_primitive_signature (CPlegacyprim p) []
	  | TIarray _ -> get_primitive_signature (CPlegacyprim p) []
	  | TIrecord id when not !Clflags.recordrepr_objarray ->  (* homogeneic records may be seen as arrays *)
	      	begin match u2.utlterm with 
		    TypUconst (TypTConst_base (Const_int nb)) -> write_record_prim id nb
		  | _ -> failwith ("Ilmcompile.Parrayset(2) : "^Clambda.typeinfo_to_string u1.utltype)
		end
	  | TIobject -> get_primitive_signature (CPlegacyprim p) [] (* WARNING!! for Obj.magic cases in rebuildmode (cf boxed_identity) *)
	  | _ ->  failwith ("Ilmcompile.Parrayset : "^Clambda.typeinfo_to_string u1.utltype)
      end

 | Praise , [u] -> get_primitive_signature (CPnewprim (TP_raise ct))  []

 (* ext-call *)
 | Pccall pd , ul ->
     let ild = try Utils.someof pd.Primitive.prim_IL  
     with _ -> Utils.bug "Ilmcompile.no primitive here" pd.Primitive.prim_name in
     let arg = match List.map (fun ilt -> of_il (fst ilt)) ild.Primitive.ilprim_sig with [] -> [CTvoid] | l -> l in 
     let ret = of_il ild.Primitive.ilprim_rt in
       get_primitive_signature (CPlegacyprim p) (ret::arg)
	 
 (* group string *)
 | Pstringlength , [u] -> get_primitive_signature (CPnewprim (TP_string_length (Ilm.ctstype_of_typeinfo TIstring))) []
 | Pstringrefu , [u1;u2] -> get_primitive_signature (CPnewprim (TP_string_ref ((Ilm.ctstype_of_typeinfo TIstring),false))) []
 | Pstringrefs , [u1;u2] -> get_primitive_signature (CPnewprim (TP_string_ref ((Ilm.ctstype_of_typeinfo TIstring),true))) []
 | Pstringsetu , [u1;u2;u3] -> get_primitive_signature (CPnewprim (TP_string_set ((Ilm.ctstype_of_typeinfo TIstring),false))) []
 | Pstringsets , [u1;u2;u3] -> get_primitive_signature (CPnewprim (TP_string_set ((Ilm.ctstype_of_typeinfo TIstring),true))) []

 (* others *)
 | Pidentity , [u] -> get_primitive_signature (CPlegacyprim p) [Ilm.ctstype_of_typeinfo rt]
 | Pignore , [u] -> get_primitive_signature (CPlegacyprim Pidentity) [CTvoid]

(* triggers the insertion of a TP_cast void, which effectively discards the value  *)

 | Pgetglobal id , [] ->
     let p' = (try 
		 let name = Ident.name id in
		   if List.mem name ["Failure";"Assert_failure";"Invalid_argument";
				     "Match_failure";"Not_found";"End_of_file";
				     "Out_of_memory";"Stack_overflow";"Sys_error";
				     "CLIinteraction.ManagedException"] then TP_get_builtin name (* builtin exception *) 
		   else TP_get_global (find_unit id) (* user globals *)
	       with Not_found -> Utils.bug "primitive" "getglobal") in
       get_primitive_signature (CPnewprim p') []
 | Poffsetref n , [u] -> get_primitive_signature (if !Clflags.rebuiltmode || !Clflags.recordrepr_objarray then CPlegacyprim p else CPnewprim (TP_offsetref n)) []
 | Pbintofint bi , [u] -> get_primitive_signature (CPnewprim (TP_convint bi)) [CTint]
 | Pintofbint bi , [u] -> get_primitive_signature (CPnewprim (TP_convint Pint32)) [bintkind_to_type bi]
 | Pcvtbint (bi1,bi2) , [u] -> get_primitive_signature (CPnewprim (TP_convint bi2)) [bintkind_to_type bi1]

 | Pintcomp comp , [u1;u2] -> (* normaly used on int, bool or char or objects in specific cases *)
     let ct' = Ilm.ctstype_of_typeinfo u1.utltype in
       if ct' = CTobject then  (* with types reconstruction sometimes int, bool & char can be hidden behind object *)
	 let pname = match comp with Ceq -> "equal" | Cneq -> "notequal" | Clt -> "lessthan" | Cgt -> "greaterthan" | Cle -> "lessequal" | Cge -> "greaterequal" in
	 let p' = (Pccall{prim_name = pname; prim_arity = 2; prim_alloc = true; 
					  prim_native_name = ""; prim_native_float = false; 
					  prim_IL=Some 
						    { ilprim_class=Ilpredef.comp_ref; ilprim_name = pname; 
						      ilprim_rt=Il.Tbool ; ilprim_virt=false;
						      ilprim_sig=[Il.Tobject,None;Il.Tobject,None] }}) in
	   select_primitive ct rt dwncstids (p',ul)
       else get_primitive_signature (CPlegacyprim p) [ct'] (* regular case, be it: int, bool or char *)

 | Pil "eq" , [u1;u2]  -> get_primitive_signature (CPnewprim TP_eq) [Ilm.ctstype_of_typeinfo u1.utltype]
 | Pil "neq" , [u1;u2]  -> get_primitive_signature (CPnewprim TP_eq) [Ilm.ctstype_of_typeinfo u1.utltype]

 | Pil "str_equal" , [u1;u2] -> get_primitive_signature (CPnewprim (TP_stringcomp Ceq)) []
 | Pil "str_nequal" , [u1;u2] -> get_primitive_signature (CPnewprim (TP_stringcomp Cneq)) []
 | Pil "str_lessthan" , [u1;u2] -> get_primitive_signature (CPnewprim (TP_stringcomp Clt)) []
 | Pil "str_greaterthan" , [u1;u2] -> get_primitive_signature (CPnewprim (TP_stringcomp Cgt)) []
 | Pil "str_lessequal" , [u1;u2] -> get_primitive_signature (CPnewprim (TP_stringcomp Cle)) []
 | Pil "str_greaterequal" , [u1;u2] -> get_primitive_signature (CPnewprim (TP_stringcomp Cge)) []

(* remainder: straightforward mapping *)
 | _ , _ -> get_primitive_signature (CPlegacyprim p) []





let insert_cast t1 t2 v =
   if t1 = t2 then v else Tprim (CPnewprim (TP_cast (t1,t2)),[v])

(* translate constant *)
let constant ti = function 
    Const_int n ->
      begin match ti with
	| TIint -> (Tconst (Tconst_int n),CTint)
	| TIbool -> (Tconst (Tconst_bool (n <> 0)),CTbool)
	| _ -> failwith ("Ilmcompile.struct_constant(1) : "^Clambda.typeinfo_to_string ti)
      end
  | Const_char c -> (Tconst (Tconst_char c),CTchar)
  | Const_string s ->
      let ct = Ilm.ctstype_of_typeinfo TIstring in
	(Tconst (Tconst_string (ct,s)),ct)
  | Const_float s  -> (Tconst (Tconst_float s),CTfloat)

(* translate structured_constant *)
let rec structured_constant ti ct = function
    TypTConst_base c -> 
      let v,ct0 = constant ti c in
	insert_cast ct0 ct v

  | TypTConst_pointer n ->
      begin match ti with
	| TIint -> insert_cast CTint ct (Tconst (Tconst_int n))
	| TIbool -> insert_cast CTbool ct (Tconst (Tconst_bool (n <> 0)))
	| TIchar -> insert_cast CTchar ct (Tconst (Tconst_char (char_of_int n)))
	| (TIunit | TIobject) when n=0  -> insert_cast CTobject ct (Tconst (Tconst_null))
	| TIvariant _ when !Clflags.variantrepr_objarray || !Clflags.noctropt -> (* means variant with cst ctors only *)
	    insert_cast CTint ct (Tconst (Tconst_int n))
	| _ -> 
	    let prim,_,rct = select_primitive ct ti [] (Pmakeblock(n,Asttypes.Immutable),[]) in
	      insert_cast rct ct (Tprim(prim,[]))
      end

  | TypTConst_block (n,scl) -> 
      begin try 
	let prim,ctl,rct = 
	  select_primitive ct ti [] (Pmakeblock(n,Asttypes.Immutable),
				   List.map (fun (sc,ti) -> {utlterm=TypUconst sc;utltype=ti} ) scl) in
	let args = List.map2 (fun (sc,ti) ct -> structured_constant ti ct sc) scl ctl in
	  insert_cast rct ct (Tprim(prim,args))
      with Invalid_argument _ -> assert false
      end
  | _ -> assert false


let rec downgrade_ulambda utl = 
  let termpart = 
    match utl.utlterm with
	TypUvar id -> TypUvar id
      | TypUconst sc -> TypUconst sc (* downgrade_structured_constant sc *)
      | TypUdirect_apply(lbl,l) -> TypUdirect_apply(lbl,List.map downgrade_ulambda l)
      | TypUgeneric_apply(t,tl) -> TypUgeneric_apply(downgrade_ulambda t,List.map downgrade_ulambda tl)
      | TypUclosure(funl,envl) -> TypUclosure(List.map (fun (lbl,ar,tids,body) -> (lbl,ar,List.map downgrade_ident tids,downgrade_ulambda body)) funl,
					      List.map downgrade_ulambda envl)
      | TypUoffset(ut,n) -> TypUoffset(downgrade_ulambda ut,n)
      | TypUlet(id,t1,t2) -> TypUlet(downgrade_ident id,downgrade_ulambda t1,downgrade_ulambda t2)
      | TypUletrec(decl,body) -> TypUletrec(List.map downgrade_ulambda_decl decl,downgrade_ulambda body)
      | TypUprim(p,tl) ->  TypUprim(p,List.map downgrade_ulambda tl)
      | TypUswitch(arg,us) ->
	  TypUswitch(downgrade_ulambda arg,
		     {tus_index_consts=us.tus_index_consts;
		      tus_actions_consts=Array.map downgrade_ulambda us.tus_actions_consts;
		      tus_index_blocks=us.tus_index_blocks;
		      tus_actions_blocks=Array.map downgrade_ulambda us.tus_actions_blocks})
      | TypUstaticfail(n,utls) -> TypUstaticfail(n,List.map downgrade_ulambda utls)
      | TypUcatch(n,tids,t1,t2) -> TypUcatch(n,List.map downgrade_ident tids,downgrade_ulambda t1,downgrade_ulambda t2)
      | TypUtrywith(t1,id,t2) -> TypUtrywith(downgrade_ulambda t1,downgrade_ident id,downgrade_ulambda t2)
      | TypUifthenelse(t,t1,t2) -> TypUifthenelse(downgrade_ulambda t,downgrade_ulambda t1,downgrade_ulambda t2)
      | TypUsequence(t1,t2) -> TypUsequence(downgrade_ulambda t1,downgrade_ulambda t2)
      | TypUwhile(t1,t2) -> TypUwhile(downgrade_ulambda t1,downgrade_ulambda t2)
      | TypUfor(id,t1,t2,dir,body) -> TypUfor(downgrade_ident id,downgrade_ulambda t1,downgrade_ulambda t2,dir,downgrade_ulambda body)
      | TypUassign(id,t) -> TypUassign(downgrade_ident id,downgrade_ulambda t)
      | TypUsend(t1,t2,tl) -> TypUsend(downgrade_ulambda t1,downgrade_ulambda t2,List.map downgrade_ulambda tl)
  in
    {utlterm = termpart; utltype = downgrade_typeinfo utl.utltype}

and downgrade_ulambda_decl (tid,t) = (downgrade_ident tid,downgrade_ulambda t)

and downgrade_ident (id,typ) = (id,downgrade_typeinfo typ)





let freeze_fun x = 
  let il = Utils.someof x.tfd_id.ilinfo in 
    x.tfd_id.ilinfo <- Some { il with ilrt = Ilm.to_il x.tfd_rt ;
				ilsig = List.map (fun v -> Ilm.to_il v.ltype , Some v.lid) x.tfd_var }
     
(* environment for compiling *)

type envt = { 
    class_cur: Il.typeref ;          (* current class name *)
    method_cur : Il.id ;             (* current method name *)
    offset_cur : int ; 

    local : (Ident.t * locl_tid) list ;        (* envt : local *)
    argmt : (Ident.t * locl_tid) list ;        (* envt : argt *)
    offset : (Il.typeref * (int * fild_tid) list) list ref ;

    argrec : (Ident.t * fild_tid) option ;

    funct : (function_label * tfundec) list ref ;  (* functions *)

    mclos : tclassdec list ref ;

    mutable closid: (Ident.t * Il.typeref) list;
    dwncstids: (Ident.t * locl_tid * bool ref) list

  }

(* initialize environment *)

let env_init unit_id = 
  Naming.init_name_table unit_id ;
  { class_cur = Ilbuild.type_ref (Naming.get_unit_id()) "Top" ; 
    method_cur = "startup" ; offset_cur = 0 ;
    local = [] ; argmt = [] ; offset = ref [] ;
    argrec = None ;
    funct = ref [] ; mclos = ref [] ;
    closid = [] ;
    dwncstids = []
  }
  
let find_closure_cid env id = List.assoc id env.closid
let add_closure_cid env id cid =
  env.closid <- (id,cid)::env.closid

    
(* manage variables *)
let create_new_local env (id,typ) ct = 
  let loc = Naming.new_local_name env.class_cur env.method_cur id in
    {lid=loc; ltype= ct} 
let create_new_local_std env (id,typ) = 
  create_new_local env (id,typ) (Ilm.ctstype_of_typeinfo typ)

let add_new_local env id tid = 
  { env with local = (id,tid) :: env.local }
let find_local env id = List.assoc id env.local 

let add_new_argmt env id tid = 
  { env with argmt = (id,tid) :: env.argmt }
let find_argmt env id = List.assoc id env.argmt

let findspecialized_ifany env id =
  try let (_,loc,_) = List.find (fun (id',_,bref) -> id = id' && !bref) env.dwncstids in
    Some loc
  with Not_found -> None

(* create a new name different from other functions of the same module *)

let create_func_id flbl = 
  match flbl.ilinfo with None -> Naming.new_func_label flbl | Some _ -> () 

let classof flbl = 
  let x = Utils.someof flbl.ilinfo in 
    Ilbuild.type_ref x.ilns x.ilname

let add_func env f = env.funct := (f.tfd_id,f) :: !(env.funct) 
let find_func env lbl = List.assoc lbl !(env.funct) 
let available_fundec env lbl = List.mem_assoc lbl !(env.funct) 
let impl_func env lbl tu = 
  try
    let fd = find_func env lbl in 
      env.funct := List.remove_assoc lbl !(env.funct) ;
      add_func env { fd with tfd_exe = tu }
  with Not_found -> Utils.bug "Ilmcompile.impl_func" lbl.opt

let add_clos env c = env.mclos := c :: !(env.mclos)

(* manage field *)
let create_new_field env id typ = 
  let name = Naming.new_field_name env.class_cur id in
  { did = (env.class_cur , name) ; dtype= Ilm.ctstype_of_typeinfo typ}

(* manage offset *)
let add_offset env cid off = env.offset := (cid,off) :: !(env.offset) 
let find_offset env cid = List.assoc cid !(env.offset) 

                           (*****************)

let rec cast t1 t2 v = 
  if t1 = t2 then v else Tprim (CPnewprim (TP_cast (t1,t2)),[v])

(* create the offset of a closure , create the names of its fields *)
let build_offset env funl ul = 
  let offset = ref (-1) in 
  let funcpart = 
    (* offset for the part with function declarations *)
    (* name of field is name of function *)
    let funoffset (lbl,ar,_,_) = 
      create_func_id lbl ; 
      let base = (Utils.someof lbl.ilinfo).ilname in 
      let fld = create_new_field env base (funlabel_typeinfo lbl.funtype) in
      let ofs = !offset+1 in 
      offset := !offset + (if ar=1 then 3 else 4) ;
      ofs , fld  in
    List.map funoffset funl in
  let envpart = 
    (* offset for the part with non functional values *)
    (* name of field is same of value if it's an ident *)
    let envoffset x = 
      let base = match x.utlterm with
          TypUvar id -> Ident.name id
        | _ -> env.class_cur.Il.trnme^"_field"  in
      let fld = create_new_field env base x.utltype in
      let ofs = !offset in 
      offset := !offset + 1 ;
      ofs , fld  in
     List.map envoffset ul in
  funcpart , envpart 

let rec simplify ulam = match ulam.utlterm with (* TODO: better if handled in primitives *)
    TypUprim(Pintcomp(Ceq) , [u1;u2])  -> begin
      match u1.utltype with 
	  TIint | TIchar | TIbool -> ulam (* 'real integer' types *) 
	| _ -> build_uprimterm (Pil "eq", [u1;u2]) ulam.utltype (* CAML pointers showing up as ints ([], exception tags ..) *)
    end
  | TypUifthenelse(u1,u2,u3) ->  let nu1 = simplify u1 in build_uifthenelseterm(nu1,u2, simplify u3) ulam.utltype
  | TypUcatch (i, ids, u1, u2) -> build_ucatchterm(i, ids, simplify u1, u2) ulam.utltype
  | _ -> ulam

                           (*****************)
	  
let rec ulambda_std env ul = ulambda_castto env (Ilm.ctstype_of_typeinfo ul.utltype) ul
and ulambda_castto env ct ul =  (* ct is expected concrete type *)
  match ul.utlterm with
      | TypUconst c -> 
	  structured_constant ul.utltype ct c

      | TypUgeneric_apply (u,ul) -> 
	  let tu = ulambda_castto env CTgenclosure u 
	  and tul = List.map (ulambda_castto env CTobject) ul in
	    cast CTobject ct (Tgeneric_apply (tu,tul))

      | TypUifthenelse ({utlterm=TypUprim (Pil "eqnull",[u1])},u2,u3) -> (* !!!! *)
	  Utils.bug "Ilmcompile" "ifthenelse -- eqnull"

      | TypUifthenelse (u1,u2,u3) -> 
	  let tu1 = ulambda_castto env CTbool u1 in
	  let tu2 = ulambda_castto env ct u2 in
	  let tu3 = ulambda_castto env ct u3 in
	    Tifthenelse (tu1,tu2,tu3)

      | TypUswitch (u,us) -> 
	  let switch_ct = match Ilm.ctstype_of_typeinfo u.utltype with
	    | CTvariant _ as swct0 -> swct0 
	    | CTint | CTchar | CTbool as swct0 -> swct0
	    | _ -> CTarray CTobject in
	  let tu = ulambda_castto env switch_ct u in
	  let tus = 
	    if Array.length us.tus_index_blocks = 0 then 
	      { ts_indexes = us.tus_index_consts;
		ts_actions = Array.map (ulambda_castto env ct) us.tus_actions_consts }
	    else begin
	      let swarg = should_specialize_variant u in 
	      let fetch_tag_table = Array.make (Array.length us.tus_index_blocks) 0 in
		Array.iteri (fun i j -> fetch_tag_table.(j) <- i) us.tus_index_blocks;
		let compile_action i uaction = 
		  match swarg with 
		      Some (id,varianttype) -> 
			let tag = fetch_tag_table.(i) in
			let is_var_used = ref false in
			let ctdwncst = (variant_specialization varianttype tag) in
			let dwncstid = create_new_local env (id,varianttype) ctdwncst in
			  (* Optim : réflechir à un moyen de le faire que si le nb d'accès est au moins 2 ... *)
			  (* ou alors post-traitement stloc i/ldloc i *)
			let action = ulambda_castto {env with dwncstids = (id,dwncstid,is_var_used)::env.dwncstids} ct uaction in
			  if !is_var_used then Tlet(dwncstid,ulambda_castto env ctdwncst u ,action) (* !! *)
			  else action
		    | None -> ulambda_castto env ct uaction
		in 
		  { ts_indexes = us.tus_index_blocks ;
		    ts_actions = Array.mapi compile_action us.tus_actions_blocks }
	    end
	  in
	    Tswitch (tu,switch_ct,tus)
	      
      | TypUstaticfail (i, ul) ->
	    let tul = List.map (ulambda_std env) ul in
	      (* here: do not push any value of type ct on the stack *)
	      Tstaticfail (i,tul)

      | TypUcatch (i,ids,u1,u2) -> 
	  let tid_list = List.map (create_new_local_std env) ids in
	    let tu1 = ulambda_castto env ct u1 in
	    let env2 = List.fold_left2 (fun env (id,_) tid -> add_new_local env id tid) env ids tid_list in 
	      let tu2 = ulambda_castto env2 ct u2 in
		Tcatch (i,tid_list,tu1,tu2)

      | TypUwhile (u1,u2) -> 
	  let tu1 = ulambda_castto env CTbool u1 in 
	  let tu2 = ulambda_castto env CTvoid u2 in
	    cast CTvoid ct (Twhile (tu1,tu2))
	      
      | TypUsend (u1, u2, []) ->
	  let tu1 = ulambda_castto env CTobject u1 in
	  let tu2 = ulambda_castto env (CTarray CTobject) u2 in
	    cast CTobject ct (Tsend(tu1,tu2,[]))
	      
      | TypUsend (u1,u2,uargs) ->
	  ulambda_castto env ct {utlterm=TypUgeneric_apply({utlterm=TypUsend(u1,u2,[]);utltype=TIgenclosure},uargs);utltype=ul.utltype} 
    
      | TypUdirect_apply (flbl,ul) ->
	  begin try  
	      if !Clflags.verbose then (Printf.printf "Direct apply : %s\n" flbl.opt;flush stdout);
	      if available_fundec env flbl then  (* true for functions defined in the current implementation file AND already compiled *)
		let fd = find_func env flbl in 
		let argt = List.map (fun x -> x.ltype) fd.tfd_var in
		let tul = List.map2 (ulambda_castto env) argt ul in
		  cast fd.tfd_rt ct (Tdirect_apply (flbl,tul)) 
	      else 
		begin
		  if flbl.ilinfo = None then 
		    (* the closure of label flbl may be defined inside one of the arguments ul (happens for some mutually recursive calls) *)
		    begin
		      (* we try to compile ul a first time to mutate the environment *)
		      let env' = {env with funct = ref !(env.funct); mclos = ref !(env.mclos); offset = ref !(env.offset)} (* to get a fresh copy *) in
 		      List.iter (fun u -> let _ = ulambda_castto env' CTobject u in ()) ul;
		      if available_fundec env' flbl then
			let fd = find_func env' flbl in 
			let argt = List.map (fun x -> x.ltype) fd.tfd_var in
			let tul = List.map2 (ulambda_castto env) argt ul in
			  cast fd.tfd_rt ct (Tdirect_apply (flbl,tul))
		      else Utils.bug "Ilmcompile.ulambda.direct_apply" flbl.opt
		    end
		  else

		    let cp = Utils.someof flbl.ilinfo in
		      
		    (* ?? *)
		    let involved_mod = cp.ilns in
		    let cui=Compilenv.get_current_unit () in 
		    let units = List.filter (function s -> (String.compare s "") <> 0)
		      (List.map (fun (x,y,_) -> if y.Il.trnme<>"" || y.Il.trnsp<>"" then x else "") cui.Compilenv.ui_imports_cmx) in
		      if not (List.mem involved_mod units) then
			begin
			  if not (List.mem involved_mod !Compilenv.current_unit_addit) then  begin (* check this !! *)
			      Compilenv.current_unit_addit := involved_mod :: !Compilenv.current_unit_addit;
			      if !Clflags.verbose then (Printf.printf "Adding new unit : %s\n" involved_mod;flush stdout)	    
			    end
			end;  
		      (* ?? *)
		      
		      let argt = List.map (fun (x,_) -> Ilm.of_il x) cp.ilsig in
		      let tul = List.map2 (ulambda_castto env) argt ul in 
			cast (Ilm.of_il cp.ilrt) ct (Tdirect_apply (flbl,tul)) 
		end
            with Not_found -> Utils.bug "Ilmcompile.ulambda" "Udirect_apply" 
	  end
	  
      | TypUfor ((id,typ),u1,u2,df,u3) -> 
	  let tu1 = ulambda_std env u1
	  and tu2 = ulambda_std env u2 in
	  let tid = create_new_local_std env (id,typ) in
	    let env2 = add_new_local env id tid in 
	    let tu3 = ulambda_castto env2 CTvoid u3 in 
	     cast CTvoid ct (Tfor (tid,tu1,tu2,df,tu3))

      | TypUtrywith (u1,(id,typ),u2) -> 
	  let tu1 = ulambda_castto env ct u1 in 
	  let tid = create_new_local_std env (id,typ) in 
	  let env2 = add_new_local env id tid in
	  let tu2 = ulambda_castto env2 ct u2 in
            Ttrywith(tu1,tid,tu2)
		
      (* default for primitive *)
      | TypUprim (p,utl) -> 
	  primitive env ul.utltype ct (p,utl)

      (* closures and shared closures *)
      | TypUclosure(fdecl,ul) -> 
	  let tu,ncid = closure env (fdecl,ul) in
	    cast (CTclosure ncid) ct tu 

      | TypUvar id -> 
	  if !Clflags.verbose then (Printf.printf "Uvar %s\n" (Ident.name id);flush stdout);    
	  begin
	    match findspecialized_ifany env id with
		None ->
		  begin
		    try let tid = find_local env id in cast tid.ltype ct (Tlocal tid)
		    with Not_found ->
		      try 
			let tid = find_argmt env id in cast tid.ltype ct (Targument tid)
		      with Not_found ->
			Utils.bug "Ilmcompile.ulambda Uvar" (Ident.unique_name id)
		  end
	      | Some tid -> cast tid.ltype ct (Tlocal tid)
	  end

      | TypUassign ((id,typ),u) ->
	  begin try 
              let tid = find_local env id in
              let tu = ulambda_castto env (Ilm.ctstype_of_typeinfo typ) u in 
		 cast CTvoid ct (Tassign (tid,tu))
            with Not_found -> Utils.bug "Ilmcompile.ulambda" "Uassign"
	  end
	
      (* simplify a offset definition *)
      | TypUlet (id1,({utlterm=TypUclosure ([_,_,_,_],[])} as u1),
              {utlterm=TypUlet(id2,{utlterm=TypUoffset({utlterm=TypUvar id3},0)},u2)}) when (fst id1) = id3 -> 
	  ulambda_castto env ct (build_uterm (TypUlet (id2,u1,u2)) u2.utltype) 
	  
      (* simplify a local definition *) 
      | TypUlet ((id,_),u1,{utlterm=TypUvar id2}) when id = id2 -> ulambda_castto env ct u1
	  
      (* special case: closure definition *)
      | TypUlet (id,{utlterm=TypUclosure(fdecl,ul)},u2) -> 
	  let tu1,ncid = closure env (fdecl,ul) in
	    (* for a closure or a shared closure *)
	    add_closure_cid env (fst id) ncid;
	    let tid = create_new_local env id (CTclosure ncid) in 
	    let env2 = add_new_local env (fst id) tid in
	    let tu2 = ulambda_castto env2 ct u2 in
	      Tlet (tid,tu1,tu2) 
		
      | TypUlet (id,u1,u2) -> (* all other cases *)
	  let tid = create_new_local_std env id in 
	  let tu1 = ulambda_castto env tid.ltype u1 in
	  let env2 = add_new_local env (fst id) tid in
	  let tu2 = ulambda_castto env2 ct u2 in
	    Tlet (tid,tu1,tu2) 

      | TypUsequence (u1,u2) -> 
	  let tu1 = ulambda_castto env CTvoid u1 in 
	  let tu2 = ulambda_castto env ct u2 in 
	    Tsequence (tu1, tu2)

      (* simplify a recursive definition *)
      | TypUletrec ([id,u1],u2) when not (Closure.occurs_var (fst id) u1) ->
	  ulambda_castto env ct (build_uterm (TypUlet (id,u1,u2)) u2.utltype)

      (* local recursive definitions : create several names *)
      | TypUletrec (decl,u) -> 
	  let vars = List.map (fun (id,_) -> create_new_local_std env id) decl in
	  let env2 = List.fold_left2 (fun e ((id,_),_) v -> add_new_local e id v) 
                       env decl vars in 
	  let tul = List.map (fun (_,u) -> ulambda_std env2 u) decl in
	  let tu = ulambda_castto env2 ct u in
	    Tletrec(List.combine vars tul,tu)

(* inside a mutually recursive fundef, to get the code pointer of an other function *)
      | TypUoffset({utlterm=TypUvar id},n) when env.argrec<>None
          && fst (Utils.someof env.argrec)=id -> 
	  begin try 
	     let tid = find_argmt env id in 
	     let (_,rid) = Utils.someof env.argrec in
	     let tu = Tprim (CPnewprim (TP_get_field rid),[Targument tid]) in
	       ( try 
		   let cid = Ilm.get_closure_repr rid.dtype in
		   let off = find_offset env cid in 
		   let fid = List.assoc (n + env.offset_cur ) off in 
		     cast fid.dtype ct (Tprim (CPnewprim (TP_get_field fid),[tu]))
		 with _ -> Utils.bug "Ilmcompile.ulambda" "Uoffset(?)" ) 
	   with Not_found -> Utils.bug "Ilmcompile.ulambda" "Uoffset (var)"
	  end

(* outside the mutually recursive fundef, in order to name functions *)
(* like: let mclos = ... in let f = (Uoffset(0) mclos) in let g=(Uoffset(4) mclos) in ...*)
(* simplifer la suite (le match) , vu que u = Uvar de toutes facons !! *)
      | TypUoffset ({utlterm=TypUvar id;utltype=utyp} as u,n) -> 
	  begin try
	     begin match utyp with 
	       | TIgenclosure | TIsharedclosure ->
		   begin try 
		     let cid = find_closure_cid env id in
		     let off = find_offset env cid in 
		     let fid = List.assoc n off in 
		     let tu = ulambda_castto env (CTclosure cid) u in 		   
		       cast fid.dtype ct (Tprim (CPnewprim (TP_get_field fid),[tu]))
		   with Not_found -> 
		     match n with 
			 0 -> (* !! *)
			   let tu = ulambda_castto env (Ilm.ctstype_of_typeinfo utyp) u in 		   
			     cast (Ilm.ctstype_of_typeinfo utyp) ct tu 
		       | k -> Utils.bug "Fgen.ulambda" ("22 Uoffset-"^(Ident.unique_name id)^"-"^(string_of_int k)^"-"^(string_of_int env.offset_cur))
		   end 
	       | _ -> Utils.bug "Fgen.ulambda" "Uoffset"  
	     end
	  with Not_found -> Utils.bug "Ilmcompile.ulambda" "Uoffset" 
	  end

      | _ -> assert false
  
and closure env = function
  (* toplevel function *) 
  | [flbl,ar,idl,u],_ when abs ar = List.length idl -> 
      (* also takes care of dummy environments, which are not used by the function *)
      (* occurs when free variables are functions that are directly applied *)
      (* in this case, there is no additional argenv parameter *)
	  create_func_id flbl ;
	  let env2 = { env with class_cur = classof flbl ; 
			 method_cur = "exec" ; argrec=None ; offset_cur = 0 } in
	  let args = List.map (create_new_local_std env2) idl in  
	  let env3 = List.fold_left2 add_new_argmt env2 (List.map fst idl) args in 
	  let tf = { tfd_id = flbl ;
                     tfd_cur = ar<0 ;
                     tfd_sts = TFStop ;
                     tfd_var = args ;
                     tfd_exe = Tnop ;
                     tfd_rt  = Ilm.ctstype_of_typeinfo (arrow_apply (funlabel_typeinfo flbl.funtype) ar)
                   } in
	    add_func env tf ;
	    impl_func env flbl (ulambda_std env3 u) ;  (* !! possible de diverger de tfd_rt ci-dessus ? *)
 	    freeze_fun tf ;
	    let ncid = classof flbl in
	      (Tprim (CPnewprim (TP_mktop ncid),[])),ncid

  (* function with an environment (closure, actually) parameter *)
  (* envt part can be empty when the function is just accessing itself via its own closure pointer *)
  (* dummy environments are captured by the first case, so here there is always a argenv parameter *)
      | [flbl,ar,idl,u] as fl,ul -> 
	  create_func_id flbl ;
	  let env2 = { env with class_cur = classof flbl ; 
			 method_cur = "exec" ; argrec=None ; offset_cur = 0 } in
	  let of1,of2 = build_offset env2 fl ul in
	    add_offset env (classof flbl) of2 ;
	    let args = List.map (create_new_local_std env2) idl in
	    let _,argenv = Utils.last_list args in  
	      argenv.ltype <-  CTclosure (classof flbl);
	      let env3 =  List.fold_left2 add_new_argmt env2 (List.map fst idl) args in 
	      let tf = { tfd_id = flbl ;
			 tfd_cur = ar<0 ;
			 tfd_sts = TFSenv (List.map snd of2) ;
			 tfd_var = args ;
			 tfd_exe = Tnop ;
			 tfd_rt  = Ilm.ctstype_of_typeinfo (arrow_apply (funlabel_typeinfo flbl.funtype) ar)
		       } in
		    add_func env tf ;
		let tul = List.map (ulambda_std env) ul in 
		  impl_func env flbl (ulambda_std env3 u) ;
		  freeze_fun tf ; 
		  let ncid = classof flbl in
		    (Tprim (CPnewprim (TP_mkenv ncid),tul)),ncid
			
  (* functions sharing a closure *) 
      | fl,ul -> 
	  let id = Naming.new_class_name "closures" "mclos" in
	  let env2 = { env with class_cur = Ilbuild.type_ref ((Naming.get_unit_id())^".closures") id } in
	  let of1,of2 = build_offset env2 fl ul in
	  let ofs = of1 @ of2 in
	    List.iter2  
              (fun (_,fid) (flbl,_,_,_) -> fid.dtype <- CTclosure (classof flbl))
              of1 fl ;
	    let flds = List.map (fun (_,fid) -> {lid=snd fid.did;ltype=fid.dtype}) ofs in
	    let ncid =  env2.class_cur in
	    let cid = { tcd_kind = CKsharedclosure ; tcd_id = ncid ; tcd_fld = flds } in 
	      add_clos env cid ;
	      add_offset env ncid ofs ;
	      let build_fun (n,fld) (flbl,ar,idl,u) = 
		let bfenv = 
		  { env with class_cur = classof flbl ; method_cur = "exec" } in
		let args  = List.map (create_new_local_std bfenv) idl in 
		let bfenv2 = List.fold_left2 add_new_argmt bfenv (List.map fst idl) args in 
		let bfenv3 = { bfenv2 with offset_cur = n } in
		  if (abs ar)<>List.length idl  (* if mutually recursive functions only call each other via Dapply, there is no argenv parameter *)
		  then ( let _,argenv = Utils.last_list args in 
			   argenv.ltype <- CTclosure (classof flbl) ) ;
		  let tf = { tfd_id = flbl ;
			     tfd_cur = ar<0 ;
			     tfd_sts = if abs ar=List.length idl then TFStop
                             else TFSrec ncid ;
			     tfd_var = args ;
			     tfd_exe = Tnop ;
			     tfd_rt  = Ilm.ctstype_of_typeinfo (arrow_apply (funlabel_typeinfo flbl.funtype) ar) ;
			   } in
		    add_func env tf ;
		    (* the rest is delayed to be executed after all mutually recursive functions have been declared *)
		    (* code that directly applies other functions makes uses of these declarations *)
		    fun () -> 
		      begin
			let bfenv4 = if (abs ar)=List.length idl then bfenv3 
			else let _,(argenv,_) = Utils.last_list idl in
			let recfld = { did = (classof flbl,"rec") ;
                                       dtype = CTclosure ncid } in
                          {bfenv3 with argrec=Some(argenv,recfld)} in
			  impl_func env flbl (ulambda_std bfenv4 u) ;
			  freeze_fun tf ; 
			  if abs ar=List.length idl then Tprim (CPnewprim (TP_mktop (classof flbl)),[])
			  else Tprim (CPnewprim (TP_mkrec (classof flbl)),[])
		      end 
	      in
	      let ftargs = List.map2 build_fun of1 fl in
	      let tul = List.map (ulambda_std env) ul in 
	      let targs = List.map (fun f -> f ()) ftargs in
		(Tprim (CPnewprim (TP_mkclos ncid),targs@tul)),ncid
			  

and primitive env ti ct (p,ul) = 
  match (p,ul) with
      (* first deal with closure-related primitives *)
    | Pfield n , [{utlterm=TypUvar id}] 
	when env.argrec<>None && fst (Utils.someof env.argrec)=id ->
	(* acces dans une cloture partagée *)
	(try 
	   let tid = find_argmt env id in 
	   let (_,rid) = Utils.someof env.argrec in
	     ( try 
		 let cid = Ilm.get_closure_repr rid.dtype in
		 let tu = Tprim ((CPnewprim(TP_get_field rid)),[Targument tid]) in
		 let off = find_offset env cid in 
		 let fid = List.assoc (n+env.offset_cur) off in
		   cast fid.dtype ct (Tprim (CPnewprim(TP_get_field fid),[tu]))
	       with _ -> Utils.bug "Fgen.ulambda" "Uoffset(!)" ) 
	 with Not_found -> Utils.bug "Ilmcompile.ulambda" "Pfield (var)")

    | Pfield n , [u] when u.utltype = TIgenclosure ->
	(* seul espoir de trouver la bonne fermeture : u est une variable ... *)
	begin match u.utlterm with 
	    TypUvar id ->
	      begin try 
		let tid = find_argmt env id in
		  (try 
		     let cid = Ilm.get_closure_repr tid.ltype in
		     let tu = ulambda_castto env tid.ltype u in 
		       (try (
			  let off = find_offset env cid in 
			  let fid = List.assoc n off in
			    cast fid.dtype ct (Tprim (CPnewprim(TP_get_field fid),[tu])) )
			with Not_found -> failwith "Ilmcompile.field(3)")
		   with _ ->  failwith "Ilmcompile.field(4)")
	      with Not_found -> failwith "Ilmcompile.field(2)"
	      end
	  | _ -> failwith "Ilmcompile.field(1)"
	end

(* hijack of CLIinteraction.ManagedException *)
    | Pfield 0 , [{utlterm = TypUprim(Pgetglobal id,[])}] when Ident.name id = "CLIinteraction" -> 
	primitive env ti ct (Pgetglobal (Ident.create "CLIinteraction.ManagedException"),[]) (* remap to a core exception access *)

    | Poffsetint _ , [u] when u.utltype = TIchar -> (* happens when matching against chars *)
	cast CTint ct (Tprim ((CPlegacyprim p),[Tprim ((CPnewprim (TP_cast(CTchar,CTint))),[ulambda_castto env CTchar u])]))

    | _ ->
	let prim,argtl,rt = select_primitive ct ti env.dwncstids (p,ul) in
	  cast rt ct (Tprim (prim,List.map2 (ulambda_castto env) argtl ul))

(* entry point *)

let rev_split = 
  let rec aux ac = function [] -> ac | (_,x)::l -> aux (x::ac) l in 
  aux [] 

let comp_unit module_id size u = 
  let env = env_init (Ident.name module_id) in 
  let t = ulambda_castto env CTvoid u in 
    (* cleanup Camil type tables *)
(*    Typemod.camil_clear_typedecls();
    Impl.clear_tables();  *)

    { tud_id = env.class_cur.Il.trnsp ;
      tud_size = size ;
      tud_fd = rev_split !(env.funct) ;
      tud_cd = List.rev !(env.mclos) ;
      tud_start = t 
    }

