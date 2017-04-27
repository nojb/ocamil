(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: compil.ml,v 1.136 2007/03/19 15:15:23 montela Exp $ *)

open Primitive 
open Lambda
open Clambda
open Ctypedlambda (* HashedPath *)
open Ilm
open Il 


(* cast *)

let exception_castto_ilrepr =    InsertCast(false,fun mt -> Inst.castclass_spec mt Ilpredef.objtab_type)

let array_castto_ilrepr typ =  InsertCast(false,fun mt -> Inst.castclass_spec mt Ilpredef.objtab_type)
let record_castto_ilrepr id =    InsertCast(false,fun mt -> Inst.castclass mt (record_class_server id))
let tuple_castto_ilrepr typl =    InsertCast(false,fun mt -> Inst.castclass_spec mt Ilpredef.objtab_type)
let sumtype_castto_ilrepr id tag =    
  (* implementation specificity: when all constructors are constant, the variant values are mapped to integers *)
  match HashedPath.find algebtypes_table id with
      NTIvariant cstrs -> 
	if (List.filter (fun (_,params) -> params<>[])  cstrs)=[] then (* only consts *)
	   InsertCast(false,fun mt -> 
			Inst.castclass mt Ilpredef.int32_ref ;
			Inst.unbox mt Ilpredef.int32_ref ;
			Inst.ldindi4 mt)
	else InsertCast(false,fun mt -> Inst.castclass mt (variant_class_server id tag))
    | _ -> failwith "ILM.sumtype_castto"


let genclosure_castto_ilrepr = InsertCast(false,fun mt -> Inst.castclass mt Ilpredef.closure_ref)


let rec insert_cast t1 t2 =
  match (t1,t2) with
      x , y when x = y -> NoCast

(* bypassing int32 types *)
    | CTint32 , _ -> insert_cast CTint t2
    | _ , CTint32  -> insert_cast t1 CTint

(* unit family *)
    | CTvoid , CTobject -> InsertCast(true,fun mt -> Inst.ldnull mt)
    | CTobject , CTvoid -> InsertCast(true,fun mt -> Inst.pop mt)
    (* note that unit is sometimes seen as an integer, via the constant 0a *) 
    | CTint , CTvoid -> InsertCast(true,fun mt -> Inst.pop mt)
    | CTvoid , CTint -> InsertCast(true,fun mt -> Inst.ldci4 mt 0)

(* other void: used inside a Usequence *)
    | _ , CTvoid -> InsertCast(true,fun mt -> Inst.pop mt)

(* string family *)
    | CTstrbld , CTstring -> 
	InsertCastWR(false,fun mt wr -> 
		       if wr=WRret then Inst.tail mt ;
		       Inst.callvirt_method mt Ilpredef.builder_conv)

    | CTarray CTchar , CTstring -> InsertCast(false,fun mt -> Inst.newobj_ctor mt Ilpredef.string_ctor)

    | CTstring , CTstrbld -> InsertCast(false,fun mt -> Inst.newobj_ctor mt Ilpredef.builder_ctor)

    | CTarray CTchar , CTstrbld -> InsertCast(false,fun mt ->
						  Inst.newobj_ctor mt Ilpredef.string_ctor;
						  Inst.newobj_ctor mt Ilpredef.builder_ctor)

    | CTstring , CTarray CTchar -> InsertCastWR(false,fun mt wr ->
						  if wr=WRret then Inst.tail mt ;
						  Inst.call_method mt Ilpredef.string_to_chararray)

    | CTstrbld , CTarray CTchar -> InsertCastWR(false,fun mt wr ->
						  Inst.callvirt_method mt Ilpredef.builder_conv;
						  if wr=WRret then Inst.tail mt ;
						  Inst.call_method mt Ilpredef.string_to_chararray)

    | (CTstrbld | CTstring | CTarray CTchar) , CTobject -> Inst.castsort_implicit false Ilpredef.object_type
    | CTobject , CTstrbld -> InsertCastWR(false,fun mt wr -> 
					    if wr=WRret then Inst.tail mt ;
					    Inst.call_method mt Ilpredef.bldstr_of_object)
    | CTobject , CTstring -> InsertCastWR(false,fun mt wr -> 
					    if wr=WRret then Inst.tail mt ;
					    Inst.call_method mt Ilpredef.string_of_object)
    | CTobject , CTarray CTchar -> InsertCastWR(false,fun mt wr -> 
					    if wr=WRret then Inst.tail mt ;
					    Inst.call_method mt Ilpredef.chararray_of_object)

(* variant family (for non-all consts case) *)
    | CTvariant _ , CTobject (* when not (is_immediate t1) always true as this point*) -> Inst.castsort_implicit false Ilpredef.object_type
    | CTobject , CTvariant (id,tag) -> sumtype_castto_ilrepr id tag
    | CTvariant (id1,_), CTvariant (id2,Some tag) when id1=id2 -> sumtype_castto_ilrepr id2 (Some tag)
(* the two following casts have to force CamIL.Variant on the stack to avoid type conflicts (in a local for instance) *)
    | CTvariant (id1,Some tag), CTvariant (id2,None) when id1=id2 -> Inst.castsort_implicit false Ilpredef.variant_type
    | CTvariant _ , CTclass tref when tref.trnme = "Variant" && tref.trnsp = "CamIL" -> 
	Inst.castsort_implicit false Ilpredef.variant_type
    | CTclass tref , CTvariant (id2, tagoption) when tref.trnme = "Variant" && tref.trnsp = "CamIL" -> 
	sumtype_castto_ilrepr id2 tagoption

(* record family *)
    | CTrecord id , CTobject -> Inst.castsort_implicit false Ilpredef.object_type
    | CTrecord id , CTclass tref -> 
	if to_il t1 = Tclass tref then NoCast else failwith (Printf.sprintf "ILM.insert_cast %s[<IL:%s>] -> %s\n" (to_string t1) (Ilm.str_class ((record_class_server id))) (to_string t2))
    | CTobject , CTrecord id -> record_castto_ilrepr id
    | CTclass tref , CTrecord id -> 
	if Tclass tref = to_il t2 then NoCast else failwith (Printf.sprintf "ILM.insert_cast %s -> %s\n" (to_string t1) (to_string t2))

(* int / int32 / bool / char family *)
    | CTint , CTobject -> 
	Inst.compose_casts (InsertCast(false,fun mt -> Inst.box mt Ilpredef.int32_ref)) 
	(Inst.castsort_implicit false Ilpredef.object_type) 
    | CTobject , CTint -> InsertCast(false,fun mt -> 
				       Inst.castclass mt Ilpredef.int32_ref ;
				       Inst.unbox mt Ilpredef.int32_ref ;
				       Inst.ldindi4 mt)
    | CTchar , CTobject -> 
	if not !Clflags.rebuiltmode then
	  Inst.compose_casts (InsertCast(false,fun mt -> Inst.box mt Ilpredef.char_ref)) (Inst.castsort_implicit false Ilpredef.object_type) 
	else Inst.compose_casts (insert_cast CTchar CTint) (insert_cast CTint CTobject)
    | CTobject , CTchar -> 
	if not !Clflags.rebuiltmode then
	  InsertCast(true,fun mt -> 
		       Inst.castclass mt Ilpredef.char_ref ;
		       Inst.unbox mt Ilpredef.char_ref ;
		       Inst.ldindi2 mt)
	else Inst.compose_casts (insert_cast CTobject CTint) (insert_cast CTint CTchar)
    | CTbool , CTobject -> 
	if not !Clflags.rebuiltmode then
	  Inst.compose_casts (InsertCast(false,fun mt -> Inst.box mt Ilpredef.bool_ref)) (Inst.castsort_implicit false Ilpredef.object_type) 
	else Inst.compose_casts (insert_cast CTbool CTint) (insert_cast CTint CTobject)
    | CTobject , CTbool -> 
	if not !Clflags.rebuiltmode then
	  InsertCast(false,fun mt -> 
		       Inst.castclass mt Ilpredef.bool_ref ;
		       Inst.unbox mt Ilpredef.bool_ref ;
		       Inst.ldindi1 mt)
	else Inst.compose_casts (insert_cast CTobject CTint) (insert_cast CTint CTbool)
(* because of CAML typing, nothing else is needed *)
    | CTint , CTchar -> Inst.castsort_implicit true Ilpredef.char_type
    | CTint , CTbool -> Inst.castsort_implicit true Ilpredef.bool_type
    | CTchar , CTint -> Inst.castsort_implicit true Ilpredef.int32_type
    | CTbool , CTint -> Inst.castsort_implicit true Ilpredef.int32_type

(* int64 family *)
    | CTint64 , CTobject -> 
	Inst.compose_casts (InsertCast(false,fun mt -> Inst.box mt Ilpredef.int64_ref)) (Inst.castsort_implicit false Ilpredef.object_type) 
    | CTobject , CTint64  -> InsertCast(false,fun mt -> 
					  Inst.castclass mt Ilpredef.int64_ref ;
					  Inst.unbox mt Ilpredef.int64_ref ;
					  Inst.ldindi8 mt)
(* nint family *)
    | CTnint , CTobject -> 
	Inst.compose_casts (InsertCast(false,fun mt -> Inst.box mt Ilpredef.nint_ref)) (Inst.castsort_implicit false Ilpredef.object_type)
    | CTobject , CTnint  -> InsertCast(false,fun mt -> 
					 Inst.castclass mt Ilpredef.nint_ref ;
					 Inst.unbox mt Ilpredef.nint_ref ;
					 Inst.ldindi mt)	
(* float family *)
    | CTfloat , CTobject -> 
	Inst.compose_casts (InsertCast(false,fun mt -> Inst.box mt Ilpredef.float64_ref)) (Inst.castsort_implicit false Ilpredef.object_type)
    | CTobject , CTfloat -> InsertCast(false,fun mt -> 
					 Inst.castclass mt Ilpredef.float64_ref ; 
					 Inst.unbox mt Ilpredef.float64_ref ;
					 Inst.ldindr8 mt)

(* closure family *)
    | CTobject , CTgenclosure -> genclosure_castto_ilrepr
    | CTgenclosure , CTclosure cid ->  InsertCast (false,fun mt -> Inst.castclass mt cid)
    | CTgenclosure , CTclass cid ->  InsertCast (false,fun mt -> Inst.castclass mt cid) (* voir d'ou ca peut provenir !! *)
    | CTclosure _ , CTgenclosure -> InsertCast(false,fun mt -> Inst.castclass mt Ilpredef.closure_ref)
    | CTobject , CTclosure cid -> InsertCast(false,fun mt -> Inst.castclass mt cid)

(* other families *)
    | CTobject , _ when not (is_immediate t2) -> 
	begin  match t2 with 
	    CTtuple typl -> tuple_castto_ilrepr typl
	  | CTarray typ -> array_castto_ilrepr typ
	  | CTexception -> exception_castto_ilrepr 
	  | CTclass cid | CTclosure cid -> InsertCast (false,fun mt -> Inst.castclass mt cid)
	  | _ -> failwith ("ILM.insert_cast CTobject -> "^(to_string t2))
	end
    | _ , CTobject when not (is_immediate t1) -> Inst.castsort_implicit false Ilpredef.object_type

    | x,y  -> 
	let warnmsg = Printf.sprintf "Warning ILM.cast %s => %s\n" (to_string x) (to_string y) in
	  print_string warnmsg;
	  InsertCast(true,fun mt -> Inst.comment mt warnmsg)

let push_dummy_value t =
  match t with
      _ when not (is_immediate t) -> InsertCast(true,fun mt -> Inst.ldnull ~of_type:(to_il t) mt)
    | CTvariant (pa,_) -> (* happens for const-only sumtypes that escape previous case *) (* CHECK: should be impossible with CTvariant ! *)
	InsertCast(true,fun mt -> Inst.ldci4 mt 0)
    | (CTint | CTint32)  -> InsertCast(true,fun mt -> Inst.ldci4 mt 0)
	

    | CTbool -> Inst.compose_casts (InsertCast(true,fun mt -> Inst.ldci4 mt 0)) (Inst.castsort_implicit true Ilpredef.bool_type)
    | CTchar -> Inst.compose_casts (InsertCast(true,fun mt -> Inst.ldci4 mt 0)) (Inst.castsort_implicit true Ilpredef.char_type)

    | CTint64  -> InsertCast(true,fun mt -> Inst.ldci8 mt Int64.zero)
    | CTnint  -> InsertCast (true,fun mt -> Inst.ldci4 mt 0 ;
			       Inst.call_method mt Ilpredef.nint_ofint) 
    | CTfloat -> InsertCast(true,fun mt -> Inst.ldcr8 mt 0.0)
    | CTvoid -> NoCast
    | _ -> Printf.printf "Warning ILM.push_dummy_value %s\n" (to_string t);NoCast
	


(*** same as Default ***)
(* for optimised cst constructors *)
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
      Ilbuild.field_ref global_const_ctr_ref ("const_ctr"^(string_of_int i)) const_ctr_type


let last_list l mes = 
 try Utils.last_list l with Failure "Utils.cut_list" -> failwith mes

type envt = 
    { wr : where_return ; 
      mt : methode ;
      recarg : string option ;
      fail : (int * (string * (string list))) list ;
      tryexc : locl_tid option;
      class_tbl : typedef list ref ;
      icu : il_compilation_unit
    }

let env_init  m cu = 
  { wr=WRret; mt=m; class_tbl= ref []; recarg=None ; fail=[] ; tryexc = None; icu=cu}

let add_fail_env env n lbl lcl_ids =
    try (ignore (List.assoc n env.fail);Utils.bug "Compil" "add_fail_env")
    with Not_found -> {env with fail = (n,(lbl,lcl_ids))::env.fail}

let get_fail_env env n = List.assoc n env.fail

let add_class env cl = env.class_tbl := cl :: !(env.class_tbl) 
let find_class env id = 
  try List.find (fun x -> x.tdnme=id) !(env.class_tbl) 
  with Not_found -> Utils.bug "Compil.find_class" id


let nyp m s = Inst.comment m ("CompilX not implemented : " ^ s)
let nyb m s =
  let cui=Compilenv.get_current_unit() in
  let str = (cui.Compilenv.ui_class).trnme ^ "::" ^ m.mnme ^ " - " ^ s in
    Utils.bug "CompilY not implemented : " str 

                         (** *************** **)

(* TODO: let the envt handle "leave" instead of soe Inst._ *)
let ending env = match env.wr with
    WRbr lbl -> ignore (Inst.br env.mt lbl)
  | WRlv lbl -> 
      Inst.remember_lbl_st_restore env.mt lbl;
      ignore (Inst.leave env.mt lbl)
  | WRret -> Inst.ret env.mt 
  | WRcont -> ()


                         (** *************** **)

let tconstant env = function
  | Tconst_null -> Inst.ldnull env.mt ; ending env
  | Tconst_int n -> Inst.ldci4 env.mt n ; ending env 
  | Tconst_char c -> Inst.ldci4 env.mt (Char.code c) ; Inst.change_stack env.mt Ilpredef.char_type ; ending env 
  | Tconst_bool b -> Inst.ldci4 env.mt (if b then 1 else 0) ; Inst.change_stack env.mt Ilpredef.bool_type ; ending env 
  | Tconst_float s -> Inst.ldcr8 env.mt (float_of_string s) ; ending env 
  | Tconst_bfloat s -> Inst.ldcr8 env.mt (float_of_string s) ; Inst.box env.mt Ilpredef.float64_ref ;ending env
  | Tconst_string (ct,s) -> 
      Inst.ldstr env.mt s;
      begin match ct with
	| CTstring -> ()
	| CTstrbld -> Inst.newobj_ctor env.mt Ilpredef.builder_ctor
	| CTarray CTchar -> Inst.call_method env.mt Ilpredef.string_to_chararray
	| _ -> assert false
      end;
      ending env

let rec rcast env rt1 rt2 t =
  match t with 
      Tprim(CPnewprim(TP_cast (rt3,rt4)) , [t2]) -> 
	begin match insert_cast rt3 rt4 with
	  | InsertCast(true,_) | InsertCastWR(true,_) -> docast env rt1 rt2 t
	  | _ -> rcast env rt3 rt2 t2
	end
    | Tprim(CPnewprim(TP_pushdummy x), [t2]) ->
	begin match push_dummy_value rt2 with
	  | InsertCast(true,_) | InsertCastWR(true,_) -> docast env rt1 rt2 t
	  | _ -> pushdummy env rt2 t2
	end
    | _ -> docast env rt1 rt2 t

and docast env rt1 rt2 t =
  match insert_cast rt1 rt2 with
      NoCast -> tlambda env t
    | InsertCast(_,cast_ops) -> 	
	tlambda {env  with wr=WRcont} t;
	cast_ops env.mt;
	ending env
    | InsertCastWR(_,cast_ops) -> 	
	tlambda {env  with wr=WRcont} t;
	cast_ops env.mt env.wr;
	ending env

and pushdummy env x t =
  match push_dummy_value x with
      NoCast -> tlambda env t
    | InsertCast(_,cast_ops) -> 	
	tlambda {env  with wr=WRcont} t;
	cast_ops env.mt;
	ending env
    | InsertCastWR(_,cast_ops) -> 	
	tlambda {env  with wr=WRcont} t;
	cast_ops env.mt env.wr;
	ending env

and primitive env = function 
  (* Integer operations *)

  | Paddint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.add env.mt ;
      ending env 

  | Psubint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.sub env.mt ;
      ending env 

  | Pnegint , [t] -> 
      tlambda {env with wr=WRcont} t ;
      Inst.neg env.mt ;
      ending env 

  | Pmulint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.mul env.mt ;
      ending env 

  | Pdivint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.div env.mt ;
      ending env 

  | Pmodint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.rem env.mt ;
      ending env 

  | Pandint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst._and env.mt ;
      ending env 

  | Porint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst._or env.mt ;
      ending env 

  | Pxorint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst._xor env.mt ;
      ending env 

  | Plslint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.shl env.mt ;
      ending env 

  | Plsrint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.shr env.mt ;
      ending env 

  | Pasrint , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.shr_un env.mt ;
      ending env 

  | Pintcomp c , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      ( match c with 
            Ceq  -> Inst.ceq env.mt 
          | Cgt  -> Inst.cgt env.mt 
          | Clt  -> Inst.clt env.mt 
          | Cneq -> Inst.ceq env.mt ;
              Inst.ldci4 env.mt 0 ;
              Inst.ceq env.mt 
          | Cge | Cle -> 
	      let lbl = Ilbuild.new_label env.mt in
	      let lbl2 = Ilbuild.new_label env.mt in 
	      let br = (if c=Cge then Inst.bge else Inst.ble) env.mt lbl in 
		Inst.ldci4 env.mt 0;
		ignore (Inst.br env.mt lbl2);
		Inst.label_force env.mt br ;
		Inst.ldci4 env.mt 1;
		Inst.label_follow env.mt lbl2
      );
      ending env 

  | Poffsetint n , [t] -> 
      tlambda {env with wr=WRcont} t ;
      Inst.ldci4 env.mt n ;
      Inst.add env.mt ;
      ending env 

  | Poffsetref n , [t] -> 
      (* implementation for Pervasives.ref as object[] *)
      (* see TP_offsetref of Pervasives.ref as specific record class *)
      tlambda {env with wr=WRcont} t ;
      Inst.dup env.mt ;
      Inst.ldci4 env.mt 0 ;
      Inst.ldelem_ref env.mt ;
      Inst.castclass env.mt Ilpredef.int32_ref ;
      Inst.unbox env.mt Ilpredef.int32_ref ;
      Inst.ldindi4 env.mt;
      Inst.ldci4 env.mt n ;
      Inst.add env.mt ;
      let reg = Ilbuild.new_register env.mt Tint32 in
	Inst.stloc_name env.mt reg ;
	Inst.ldci4 env.mt 0 ;
	Inst.ldloc_name env.mt reg ;
	Ilbuild.release_loc env.mt reg ;
	Inst.box env.mt Ilpredef.int32_ref ;
	Inst.stelem_ref env.mt ;
	ending env 

  (* Boolean operations *)
  | Psequand , [t1;t2] -> 
      let lbl1 = Ilbuild.new_label env.mt in
      let lbl2 = if env.wr=WRcont then Ilbuild.new_label env.mt else lbl1 in
	tlambda {env with wr=WRcont} t1 ; 
	let br = Inst.brtrue env.mt lbl1 in
	  Inst.ldci4 env.mt 0 ;
	  if env.wr=WRcont then ignore (Inst.br env.mt lbl2) else ending env ;
	  Inst.label_force env.mt br ;
	  tlambda {env with wr=WRcont} t2 ; 
	  if env.wr=WRcont then Inst.label_follow env.mt lbl2 ;
	  ending env

  | Psequor , [t1;t2] -> 
      let lbl1 = Ilbuild.new_label env.mt in
      let lbl2 = if env.wr=WRcont then Ilbuild.new_label env.mt else lbl1 in
	tlambda {env with wr=WRcont} t1 ; 
	let br = Inst.brfalse env.mt lbl1 in
	  Inst.ldci4 env.mt 1 ;
	  if env.wr=WRcont then ignore (Inst.br env.mt lbl2) else ending env ;
	  Inst.label_force env.mt br ;
	  tlambda {env with wr=WRcont} t2 ; 
	  if env.wr=WRcont then Inst.label_follow env.mt lbl2 ;
	  ending env

  | Pnot , [t] -> 
      tlambda {env with wr=WRcont} t ;
      Inst.ldci4 env.mt 0 ;
      Inst.ceq env.mt ;
      ending env 

  (* Float operations *)
  | Paddfloat , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.add env.mt ;
      ending env 

  | Psubfloat , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.sub env.mt ;
      ending env 

  | Pnegfloat , [t] -> 
      tlambda {env with wr=WRcont} t ;
      Inst.neg env.mt ;
      ending env 

  | Pabsfloat , _ -> nyb env.mt "absfloat" (* now handled by some Pccall *)

  | Pmulfloat , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.mul env.mt ;
      ending env 

  | Pdivfloat , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.div env.mt ;
      ending env 

  | Pfloatcomp c , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      ( match c with 
            Ceq  -> Inst.ceq env.mt 
          | Cgt  -> Inst.cgt env.mt 
          | Clt  -> Inst.clt env.mt 
          | Cneq -> Inst.ceq env.mt ;
              Inst.ldci4 env.mt 0 ;
              Inst.ceq env.mt 
          | Cge -> Inst.clt env.mt; (* there is only strict inequality *)
	      Inst.ldci4 env.mt 0 ;
              Inst.ceq env.mt
          | Cle -> Inst.cgt env.mt;
	      Inst.ldci4 env.mt 0 ;
              Inst.ceq env.mt
       );
      ending env 

  | Pintoffloat , _ -> nyb env.mt "intoffloat" (* now handled by some Pccall *)
  | Pfloatofint , _ -> nyb env.mt "floatofint" (* now handled by some Pccall *)

  (* Operations on heap blocks *)
  | Pmakeblock (tag,_) , tl -> 
      if (List.length tl)=0 && not (!Clflags.noctropt)  then
	begin (* optmimising constant constructors *) 
          if tag >= 256 then Utils.bug "Inst.TP_mkarray optim" ">=256 !!";
	  Inst.ldfld_field env.mt (global_const_ctr_field tag) NoInstance;
	  ending env
	end
      else begin
      Inst.ldci4 env.mt ((List.length tl)+1) ;
      Inst.newarr env.mt Ilpredef.object_ref ;
      let i = ref 0 in 
      let f t = 
        Inst.dup env.mt ;
        Inst.ldci4 env.mt !i ; 
        tlambda {env with wr=WRcont} t ;
        Inst.stelem_ref env.mt ;
        incr i 
      in
	List.iter f tl ;
	Inst.dup env.mt ;
	Inst.ldci4 env.mt !i ; 
	Inst.ldci4 env.mt tag ; 
	Inst.box env.mt Ilpredef.int32_ref ;
	Inst.stelem_ref env.mt ;
	ending env
      end

(* Array operations *)
  | Pmakearray ak , tl -> 
      Inst.ldci4 env.mt ((List.length tl)+1) ;
      Inst.newarr env.mt Ilpredef.object_ref ;
      let i = ref 0 in 
      let f t = 
        Inst.dup env.mt ;
        Inst.ldci4 env.mt !i ; 
        tlambda {env with wr=WRcont} t ;
        Inst.stelem_ref env.mt ;
        incr i 
      in
	List.iter f tl ;
	Inst.dup env.mt ;
	Inst.ldci4 env.mt !i ; 
	Inst.ldci4 env.mt 0 ; 
	Inst.box env.mt Ilpredef.int32_ref ;
	Inst.stelem_ref env.mt ;
	ending env

  | Parraylength ak , [t] -> 
      tlambda {env with wr=WRcont} t ;
      Inst.ldlen env.mt ;
      Inst.ldci4 env.mt 1; (* arrays have tag 0 ... *)
      Inst.sub env.mt;
      ending env 

  (* TODO: must code safe versions separately (make them throw the expected exception) *)
  | (Parrayrefu ak | Parrayrefs ak) , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.ldelem_ref env.mt ;
      ending env 
	
  | (Parraysetu ak | Parraysets ak) , [t1;t2;t3] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      tlambda {env with wr=WRcont} t3 ;
      Inst.stelem_ref env.mt ;
      ending env 	


  (* Exceptions *)
  | Praise , _ -> Utils.bug "Compint.primitive" "raise deprecated"

  (* External call *)
  | Pccall pd , tl -> 
      let p = Utils.someof pd.prim_IL in
      let cn = p.ilprim_class and mnme = p.ilprim_name in 
	List.iter (tlambda {env with wr=WRcont}) tl ;
	if env.wr=WRret then Inst.tail env.mt ;
	if p.ilprim_virt 
	then Inst.callvirt env.mt cn mnme p.ilprim_rt (List.tl p.ilprim_sig) 
	else Inst.call env.mt cn mnme p.ilprim_rt p.ilprim_sig NoInstance ;
	ending env 

  | Pisint, [t] -> nyb env.mt "isint"

  |  Pisout, [t1; t2]  -> 
       tlambda {env with wr=WRcont} t1 ; 
       tlambda {env with wr=WRcont} t2 ; 
       Inst.clt_un env.mt ; 
       ending env        

(* big ints *)
  | Paddbint bi , ([t1;t2] as al) -> primitive env (Paddint,al)
  | Psubbint bi , ([t1;t2] as al) -> primitive env (Psubint,al)
  | Pnegbint bi , ([t] as al) -> primitive env (Pnegint,al)
  | Pmulbint bi , ([t1;t2] as al) -> primitive env (Pmulint,al)
  | Pdivbint bi , ([t1;t2] as al) -> primitive env (Pdivint,al)
  | Pmodbint bi , ([t1;t2] as al) -> primitive env (Pmodint,al)
  | Pandbint bi , ([t1;t2] as al) -> primitive env (Pandint,al)
  | Porbint bi , ([t1;t2] as al) -> primitive env (Porint,al)
  | Pxorbint bi , ([t1;t2] as al) -> primitive env (Pxorint,al)
  | Plslbint bi , ([t1;t2] as al) -> primitive env (Plslint,al)
  | Plsrbint bi , ([t1;t2] as al) -> primitive env (Plsrint,al)
  | Pasrbint bi , ([t1;t2] as al) -> primitive env (Pasrint,al)
  | Pbintcomp(bi,c) , ([t1;t2] as al) -> primitive env (Pintcomp c,al)

(* big arrays : nope !*)

(* misc. *)
  | Pidentity , [t] -> tlambda env t 
 
(* Pignore: handled by Pidentity and TP_cast void *)

  (* Globals *)
  | Pgetglobal _ , _ -> Utils.bug "Compil.primitive" "get_global deprecated"
  | Psetglobal _ , _ -> Utils.bug "Compil.primitive" "set_global deprecated"

  (* Bitvect operations *)
  | Pbittest , _ -> Utils.bug "Compil.primitive" "bittest deprecated"

  (* CamIL primitives *)

 | Pil s , _ -> nyb env.mt ("Pil primitive "^s)

 | (p,_) ->
     Printlambda.primitive Format.std_formatter p;
     Utils.bug "Compil.primitive" "bad number of arg"


and tprimitive env = function 

  | TP_offsetref n , [t] -> 
      (* implementation for Pervasives.ref as specific record class *)
      (* see Poffsetref of Pervasives.ref as object[] *)
      tlambda {env with wr=WRcont} t ;
      Inst.dup env.mt ;
      let typeref = Ilbuild.type_ref "Pervasives" "ref" in
      let fldref = Ilbuild.field_ref typeref "contents" Tobject in
	Inst.ldfld env.mt fldref;
	Inst.castclass env.mt Ilpredef.int32_ref ;
	Inst.unbox env.mt Ilpredef.int32_ref ;
	Inst.ldindi4 env.mt;
	Inst.ldci4 env.mt n ;
	Inst.add env.mt ;
	Inst.box env.mt Ilpredef.int32_ref ;
	Inst.stfld env.mt fldref;
	ending env 

  | TP_pushint n , [] -> 
      Inst.ldci4 env.mt n ;
      ending env

  | TP_buildobject (cid,fullctrsig,flds) , tl ->
      let env2 = {env with wr=WRcont} in 
	List.iter (tlambda env2) tl;
	Inst.newobj env.mt cid ".ctor" Il.Tvoid fullctrsig;
	ending env
	  
  | TP_get_global id , [] -> 
      let typr = Ilbuild.type_ref id "Top" in (* !! *)
      Inst.ldsfld env.mt (Ilbuild.field_ref typr "global" Ilpredef.objtab_type) ;
      ending env 

  | TP_mktop cid , [] -> 
      Inst.newobj_class env.mt (find_class env cid.trnme) ;
      ending env 

  | TP_mkenv cid , tl  -> 
      List.iter (tlambda {env with wr=WRcont}) tl ;
      Inst.newobj_class env.mt (find_class env cid.trnme) ;
      ending env 

  | TP_mkrec cid , [] -> 
      Inst.ldloc_name env.mt (Utils.someof env.recarg) ;
      Inst.newobj_class env.mt (find_class env cid.trnme) ;
      ending env 

  | TP_mkclos cid , tl -> 
(* using .ctor() with no argument *)
      let cl = find_class env cid.trnme in 
      let reg = Ilbuild.new_register env.mt (Tclass cid) in 
	Inst.newobj_class env.mt cl ;
	Inst.stloc_name env.mt reg ;
	let env2 = {env with wr=WRcont ; recarg=Some reg} in 
	let comp_args cl t fdr =
	  let fd=Ilbuild.fieldref_of_fielddef cl fdr in
          Inst.ldloc_name env2.mt reg ;
          tlambda env2 t ;
          Inst.stfld_field env2.mt fd Instance in
	  List.iter2 (comp_args cl) tl cl.tdfld ;
	  Inst.ldloc_name env.mt reg ;
	  Ilbuild.release_loc env.mt reg ;
	  ending env 

  | TP_get_field fid , [] -> (* no argument => must be a static field *)
      let fldref = Ilbuild.field_ref (fst fid.did) (snd fid.did) (Ilm.to_il fid.dtype) in
	Inst.ldsfld env.mt fldref;
	ending env 
	
  | TP_get_field fid , [t] -> 
      tlambda {env with wr=WRcont} t ;
      let fldref = Ilbuild.field_ref (fst fid.did) (snd fid.did) (Ilm.to_il fid.dtype) in
	Inst.ldfld env.mt fldref;
	ending env 

  | TP_set_field fid , [t1;t2]  ->  
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      let fldref = Ilbuild.field_ref (fst fid.did) (snd fid.did) (Ilm.to_il fid.dtype) in
	Inst.stfld env.mt fldref;
	ending env

  | TP_get_block n , [t] -> 
      tlambda {env with wr=WRcont} t ;
      Inst.ldci4 env.mt n ;
      Inst.ldelem_ref env.mt ;
      ending env 
	
  | TP_set_block n , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      Inst.ldci4 env.mt n ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.stelem_ref env.mt  ;
      ending env 	

  | TP_string_length ct , [t] ->   
      tlambda {env with wr=WRcont} t ;
      begin match ct with 
	| CTstrbld ->
	    if env.wr=WRret then Inst.tail env.mt ;
	    Inst.callvirt_method env.mt Ilpredef.builder_length
	| CTstring ->
	    if env.wr=WRret then Inst.tail env.mt ;
	    Inst.callvirt_method env.mt Ilpredef.builder_length
	| CTarray CTchar -> Inst.ldlen env.mt
	| _ -> assert false
      end;
      ending env

(* TODO: implement safemode *)
  | TP_string_ref(ct,safemode) , [t1;t2] ->
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      begin match ct with
	| CTstrbld ->
	    if env.wr=WRret then Inst.tail env.mt ;
	    Inst.callvirt_method env.mt Ilpredef.builder_getchars
	| CTstring ->
	    if env.wr=WRret then Inst.tail env.mt ;
	    Inst.callvirt_method env.mt Ilpredef.string_getchars
	| CTarray CTchar ->
	    Inst.ldelem_char env.mt
	| _ -> assert false
      end;
      ending env

  | TP_string_set(ct,safemode) , [t1;t2;t3] ->
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      tlambda {env with wr=WRcont} t3 ;
      begin match ct with
	| CTstrbld ->
	    if env.wr=WRret then Inst.tail env.mt ;
	    Inst.callvirt_method env.mt Ilpredef.builder_setchars
	| CTstring ->
	    (* TODO: use warning system instead ! *)
	    print_endline "String set instruction encountered in immutable mode";
	    Inst.pop env.mt;
	    Inst.pop env.mt;
	    Inst.pop env.mt;
	    Inst.call_method env.mt Ilpredef.string_setchars
	| CTarray CTchar ->
	    Inst.stelem_char env.mt
	| _ -> assert false
      end;
      ending env

  | TP_stringcomp Ceq , [t1;t2] ->
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.call_method env.mt Ilpredef.string_compare;
      Inst.ldci4 env.mt 0;
      Inst.ceq env.mt;
      ending env

  | TP_stringcomp Cneq , [t1;t2] ->
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.call_method env.mt Ilpredef.string_compare;
      Inst.ldci4 env.mt 0;
      Inst.ceq env.mt;
      Inst.ldci4 env.mt 0;
      Inst.ceq env.mt;
      ending env

  | TP_stringcomp Clt , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.call_method env.mt Ilpredef.comp_lessthan;
      ending env
  | TP_stringcomp Cle , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.call_method env.mt Ilpredef.comp_lessequal;
      ending env
  | TP_stringcomp Cgt , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.call_method env.mt Ilpredef.comp_greaterthan;
      ending env
  | TP_stringcomp Cge , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.call_method env.mt Ilpredef.comp_greaterequal;
      ending env

  | TP_raise ct, [t] ->
      begin
	match t,env.tryexc with 
	  | (Tlocal tid,Some exntid) when (tid = exntid) ->
	      Inst.rethrow env.mt
	  | _,_ ->
	      tlambda {env with wr=WRcont} t ;
	      Inst.call_method env.mt Ilpredef.exn_raise
      end;
      pushdummy env ct Tnop

  | TP_convint dest , [t] ->
      tlambda {env with wr=WRcont} t ;     
      let convinst = match dest with
	  Lambda.Pint32  -> Inst.convi4
	| Lambda.Pint64 -> Inst.convi8
	| Lambda.Pnativeint -> Inst.convi
      in
	convinst env.mt;
	ending env

  | TP_get_builtin s ,[] -> 
      Inst.ldfld_field env.mt 
      ( match s with 
            "Failure" -> Ilpredef.cst_failure 
	  | "Assert_failure" -> Ilpredef.cst_assertfailure
          | "Match_failure" -> Ilpredef.cst_matchfail
          | "Invalid_argument" -> Ilpredef.cst_invargt 
	  | "Not_found" -> Ilpredef.cst_notfound
	  | "End_of_file" -> Ilpredef.cst_endoffile 
	  | "Out_of_memory" -> Ilpredef.cst_outofmemory
	  | "Stack_overflow" -> Ilpredef.cst_stackoverflow
	  | "Sys_error" -> Ilpredef.cst_syserror

	  | "CLIinteraction.ManagedException" -> Ilpredef.cst_managedexception
	  | _ -> nyb env.mt ("unknown_builtin : " ^ s)  ) NoInstance ;
      ending env 
	
  | TP_cast (x,y) , [t] -> rcast env x y t 

  | TP_pushdummy x , [t] -> pushdummy env x t

  | TP_eq , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.ceq env.mt ; 
      ending env 

  | TP_neq , [t1;t2] -> 
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.ceq env.mt ;
      Inst.ldci4 env.mt 0 ;
      Inst.ceq env.mt ;
      ending env 
	
  | (_,_) -> nyb env.mt "tprimitive"


and tlambda env = function
  | Tconst c -> 
      tconstant env c

  | Tdirect_apply (lb,tl) -> 
      List.iter (tlambda {env with wr=WRcont}) tl ;
      let il = Utils.someof lb.ilinfo in 
      (* optimizing tailcalls to self *)
	if env.wr=WRret && (
	    let mc = List.filter (fun c -> c.tdnme = il.ilname && c.tdnsp = il.ilns) !(env.class_tbl) in
	      mc <> [] && List.exists (fun meth -> meth == env.mt) (List.hd mc).tdmet) then (* can optimize *)
	  (
	    for i = (List.length tl)-1 downto 0 do Inst.starg env.mt i done;
	    ignore (Inst.br env.mt (Ilbuild.init_label env.mt))
	  )
	else (
	    if env.wr=WRret then Inst.tail env.mt ;
	    Inst.call env.mt (Ilbuild.type_ref il.ilns il.ilname) "exec" il.ilrt il.ilsig NoInstance ;
	    ending env 
	  )
	  
  | Tgeneric_apply (t1,[t2]) ->
      tlambda {env with wr=WRcont} t1 ;
      tlambda {env with wr=WRcont} t2 ;
      if env.wr=WRret then Inst.tail env.mt ;
      Inst.callvirt_method env.mt Ilpredef.closure_apply ;
      ending env 

  | Tgeneric_apply (t1,t2::tl) -> 
      let ap1 = Tgeneric_apply (t1,[t2]) in 
      let cap1 = Tprim (CPnewprim(TP_cast (CTobject,CTgenclosure)),[ap1]) in
	tlambda env (Tgeneric_apply (cap1,tl)) 

  | Tlet(tid,t1,t2) when tid.ltype = CTvoid ->
      tlambda {env with wr=WRcont} t1 ; 
      tlambda env t2 

  | Tlet (tid,t1,t2) -> 
      let id = tid.lid in 
	tlambda {env with wr=WRcont} t1 ; 
	Inst.new_local env.mt id (Ilm.to_il tid.ltype) ;
	Inst.stloc_name env.mt id ;
	tlambda env t2 ; 
	Ilbuild.release_loc env.mt id

  | Tletrec (itl,t) ->  
      (* Whereas tlambda t outputs the code for t, such that the evaluation of tlambda t leaves its result on the top of the stack, *)
      (* the code computed by delayed_tlambda uses a local variable to retrieve an empty structure suitable to the result (given by letrec), *)
      (* and insert the result in that structure, leaving the stack unchanged *)
      let loc (tid,_) = 
        Inst.new_local env.mt tid.lid (Ilm.to_il tid.ltype) in 
	List.iter loc itl ;
	let comp1 (tid,code) = 
          let pass2 = delayed_tlambda {env with wr=WRcont} code in
	    (* the call push the empty structure on the stack, the filling-in code being returned in pass2 *)
          let var = tid.lid in
            Inst.stloc_name env.mt var ;
            var , Ilm.to_il tid.ltype , pass2 
	and comp2 (v,vt,p) = p v  in
	  (* At this stage all mutually recursive structures are allocated and initialisation codes can be called *)
	  List.iter comp2 (List.map comp1 itl) ;
	  tlambda env t 

  | Tprim (CPlegacyprim p,tl) -> primitive env (p,tl)
  | Tprim (CPnewprim p,tl) -> tprimitive env (p,tl) 

  | Tswitch (t,swct,ts) ->
      tlambda {env with wr=WRcont} t ;
      switch env ts.ts_indexes ts.ts_actions swct 

  | Tstaticfail (i,tl) -> 
      let (lbl,listloc) = get_fail_env env i in
	List.iter2 (fun loc t -> 
		     tlambda {env with wr=WRcont} t;
		     Inst.stloc_name env.mt loc
		  ) listloc tl ;  
	ignore (Inst.br_for_staticfail env.mt lbl) 

  | Tcatch (i,ids,t1,t2) -> 
      let lbl = Ilbuild.new_label env.mt in
      let lbl2 = if env.wr=WRcont then Ilbuild.new_label env.mt else lbl in 
      let env2 = if env.wr=WRcont then { env with wr=WRbr lbl2} else env in
      let st = Inst.dump_stack env.mt in 
      let listlocl = List.map (fun id -> Inst.new_local env.mt id.lid (Ilm.to_il id.ltype)) ids in
	tlambda (add_fail_env env2 i lbl (List.map (fun x -> x.lid) ids)) t1;
	Inst.label_force env.mt (lbl,st) ;
	tlambda env2 t2 ;
	if env.wr=WRcont then Inst.label_follow env.mt lbl2

  | Ttrywith (t1,tid,t2) -> 
      let last_wr = env.wr in 
      let lbl1 = Ilbuild.new_label env.mt in
      let stack = Inst.register_stack env.mt in  
	Inst.try_main env.mt ;
        tlambda {env with wr=WRlv lbl1} t1 ;
	Inst.try_catch env.mt ;
        let id = tid.lid in
          Inst.new_local env.mt id (Ilm.to_il tid.ltype) ;
 	  Inst.force_stack env.mt (if !Clflags.noILexceptionHandling then Ilpredef.exn_type 
 				   else Ilpredef.sysexn_type);
 	  (* when entering a catch block, the exception value is automatically pushed on the stack by the execution system *)
 	  if not !Clflags.noILexceptionHandling then begin
	  let lbl_embedexc = Ilbuild.new_label env.mt 
	  and lbl_camilexc = Ilbuild.new_label env.mt in
	    Inst.dup env.mt ;
            Inst.isinst env.mt Ilpredef.exn_ref ;
	    ignore (Inst.brfalse env.mt lbl_embedexc);
	    Inst.castclass env.mt Ilpredef.exn_ref;
	    ignore (Inst.br env.mt lbl_camilexc);
	    Inst.label_follow env.mt lbl_embedexc ;
	    Inst.call_method env.mt Ilpredef.exn_embed ;
	    Inst.label_follow env.mt lbl_camilexc 
	  end;
	  Inst.ldfld_field env.mt Ilpredef.exn_field Instance;
          Inst.stloc_name env.mt id ;
          tlambda {env with wr=WRlv lbl1; tryexc=Some tid } t2 ;
          Ilbuild.release_loc env.mt id ;
	  Inst.try_end env.mt ;
	  Inst.label_follow env.mt lbl1 ;
	  (match env.mt.mcin.stck with 
	       []  -> 
		   if stack<>[] then Inst.stack_register env.mt stack ;
		 Inst.execute_lbl_st_restore env.mt lbl1;
	     |  _  -> Utils.bug "Compil.tlambda de RAF" "Utrywith : la pile devrait etre vide" ) ;
	  ending env
	    
  | Tifthenelse (t1,t2,t3) -> 
      let lbl = Ilbuild.new_label env.mt in
      let lbl2 = if env.wr=WRcont then Ilbuild.new_label env.mt else lbl in 
	tlambda {env with wr=WRcont} t1 ;
	let br = Inst.brfalse env.mt lbl in 
	  tlambda (if env.wr=WRcont then {env with wr=WRbr lbl2} else env) t2 ;
	  Inst.label_force env.mt br ;
	  tlambda env t3 ;
	  if env.wr=WRcont then Inst.label_follow env.mt lbl2 
	  
  | Tifvar (t1,t2,t3) -> 
      let lbl = Ilbuild.new_label env.mt in
      let lbl2 = if env.wr=WRcont then Ilbuild.new_label env.mt else lbl in 
	tlambda {env with wr=WRcont} t1 ;
	let br = Inst.brfalse env.mt lbl in 
	  tlambda (if env.wr=WRcont then {env with wr=WRbr lbl2} else env) t2 ;
	  Inst.label_force env.mt br ;
	  tlambda env t3 ;
	  if env.wr=WRcont then Inst.label_follow env.mt lbl2 

  | Tsequence (t1,t2) -> 
      tlambda { env with wr=WRcont} t1 ;
      tlambda env t2 

  | Twhile (t1,t2) -> 
      let lbl = Ilbuild.new_label env.mt in
      let lbl2 = match env.wr with WRbr x -> x | _ -> Ilbuild.new_label env.mt in
	Inst.label_follow env.mt lbl ;
	tlambda {env with wr=WRcont} t1 ;
	let br = Inst.brfalse env.mt lbl2 in
	  tlambda {env with wr=WRcont} t2 ;
	  ignore (Inst.br env.mt lbl) ;
	  ( match env.wr with 
		WRbr _ -> ()
              | _ -> Inst.label env.mt br ; ending env )

  | Tfor (tid,t1,t2,df,t3) -> 
      let lbl = Ilbuild.new_label env.mt in
      let lbl2 = match env.wr with WRbr x -> x | _ -> Ilbuild.new_label env.mt in
      let id =  tid.lid in
	tlambda {env with wr=WRcont} t1 ;
	Inst.new_local env.mt id (Ilm.to_il tid.ltype) ;
	Inst.stloc_name env.mt id  ;
	tlambda {env with wr=WRcont} t2 ;
	let reg = Ilbuild.new_register env.mt Tint32 in
	  Inst.stloc_name env.mt reg  ;
	  Inst.label_follow env.mt lbl ;
	  Inst.ldloc_name env.mt reg ;
	  Inst.ldloc_name env.mt id ;
	  let br = match df with 
              Asttypes.Upto -> Inst.blt env.mt lbl2 
            | Asttypes.Downto -> Inst.bgt env.mt lbl2 in
	    tlambda {env with wr=WRcont} t3 ;
	    Inst.ldloc_name env.mt id ;
	    Inst.ldci4 env.mt 1 ;
	    ( match df with 
		  Asttypes.Upto -> Inst.add env.mt
		| Asttypes.Downto -> Inst.sub env.mt ) ;
	    Inst.stloc_name env.mt id ;
	    ignore (Inst.br env.mt lbl) ;
	    Ilbuild.release_loc env.mt id ;
	    Ilbuild.release_loc env.mt reg ;
	    ( match env.wr with 
		  WRbr _ -> ()
		| _ -> Inst.label env.mt br ; ending env )

  | Tassign (tid,t) -> 
      let id = tid.lid in
	tlambda {env with wr=WRcont} t ;
	Inst.stloc_name env.mt id ;
	ending env 

  | Tsend (t1,t2,[]) ->
      Inst.comment env.mt "begin send" ;
      tlambda {env with wr=WRcont} t2 ;
      Inst.castclass_spec env.mt Ilpredef.objtab_type ; 
      let reg = Ilbuild.new_register env.mt Ilpredef.objtab_type in
	Inst.stloc_name env.mt reg ;
	Inst.ldloc_name env.mt reg ;
	Inst.ldci4 env.mt 0 ;
	Inst.ldelem_ref env.mt ;
	Inst.castclass_spec env.mt Ilpredef.objtab_type ;
	
	(* computing the label *)
	tlambda {env with wr=WRcont} t1 ;
	Inst.castclass env.mt Ilpredef.int32_ref; (* the return value of CamlInternalOO.new_method has type object and we wqnt to downcast it *)
	Inst.unbox env.mt Ilpredef.int32_ref ;
	Inst.ldindi4 env.mt;
	
	let label = Ilbuild.new_register env.mt Ilpredef.int32_type in
	  Inst.stloc_name env.mt label ;
	  (* The following IL code mirrors CamlinternalOO.decode *)
	  (* in which we set: step=2 (as Sys.word_size/16) and bucket_size=32 *)
	  Inst.ldloc_name env.mt label ;
	  Inst.ldci4 env.mt 16;
	  Inst.shr_un env.mt; (* / 65536 *)
	  Inst.ldci4 env.mt 2;
	  Inst.div env.mt; (* / step *)
	  
	  Inst.ldelem_ref env.mt ;
	  Inst.castclass_spec env.mt Ilpredef.objtab_type ;
	  
	  Inst.ldloc_name env.mt label ;
	  Inst.ldci4 env.mt 64; 
	  Inst.rem env.mt; (* modulo (step * bucket_size) *)
	  Inst.ldci4 env.mt 2;
	  Inst.div env.mt ;  (* /step *)
	  
	  Inst.ldelem_ref env.mt ;
	  Inst.castclass env.mt Ilpredef.closure_ref ;
	  Inst.ldloc_name env.mt reg ;
	  Inst.callvirt_method env.mt Ilpredef.closure_apply ;
	  ending env

  | Tsend (_,_,_) -> nyb env.mt "send"

  | Tlocal tid -> 
      Inst.ldloc_name env.mt tid.lid ;
      ending env 

  | Targument tid -> 
      Inst.ldarg_name env.mt tid.lid ;
      ending env 

  | Tnop -> 
      ending env 

  | _ -> 
      let cui=Compilenv.get_current_unit() in
	Utils.bug "Compil.tlambda" ((cui.Compilenv.ui_class).trnme^"::"^env.mt.mnme)


and delayed_tlambda env = 
  (* caution: cannot use local names that are not defined yet *)
  (* we first allocate and then evaluate *)
  function
  | Tconst _ -> Utils.bug "Compil.delayed_tlambda" "Tconst"
  | Tdirect_apply (_,_) -> Utils.bug "Compil.delayed_tlambda" "Tdirect_apply"
  | Tgeneric_apply (_,_)-> Utils.bug "Compil.delayed_tlambda" "Tgeneric_apply"

  | Tifthenelse (_,_,_) -> Utils.bug "Compil.delayed_tlambda" "Tifthenelse"
  | Tsequence (t1,t2) -> 
      let res = delayed_tlambda {env with wr=WRcont} t2 in
      ( fun v ->
	tlambda { env with wr=WRcont} t1 ;
	res v )
  | Twhile (_,_) -> Utils.bug "Compil.delayed_tlambda" "Twhile"
  | Tfor (tid,t1,t2,df,t3) -> Utils.bug "Compil.delayed_tlambda" "Tfor"

  | Tstaticfail (_,_) -> Utils.bug "Compil.delayed_tlambda" "Tstaticfail"
  | Tcatch (_,_,_,_) -> Utils.bug "Compil.delayed_tlambda" "Tcatch"
  | Tlocal _ -> Utils.bug "Compil.delayed_tlambda" "Tlocal"
  | Targument _ -> Utils.bug "Compil.delayed_tlambda" "Targument"
  | Tswitch _ -> Utils.bug "Compil.delayed_tlambda" "Tswitch"

  | Tlet (tid,t1,t2) -> 
      let res = delayed_tlambda {env with wr=WRcont} t2 in
	( fun v ->
            let id = tid.lid in
              tlambda {env with wr=WRcont} t1 ;
              Inst.new_local env.mt id (Ilm.to_il tid.ltype) ;
              Inst.stloc_name env.mt id ;
              res v ;
              Ilbuild.release_loc env.mt id )

  | Tletrec (itl,t) ->  
      Utils.bug "Compil.delayed_tlambda Tletrec" "not implemented"

  | Tprim (CPlegacyprim (Lambda.Pmakeblock (tag,_)),tl) -> 
      Inst.ldci4 env.mt ((List.length tl)+1) ;
      Inst.newarr env.mt Ilpredef.object_ref ;
      ( fun v -> 
          let i = ref 0 in 
          let fillin t = 
            Inst.ldloc_name env.mt v;
            Inst.ldci4 env.mt !i ; 
            tlambda {env with wr=WRcont} t ;
            Inst.stelem_ref env.mt ;
            incr i 
	  in
            List.iter fillin tl ;
            Inst.ldloc_name env.mt v ;
            Inst.ldci4 env.mt !i ; 
            Inst.ldci4 env.mt tag ; 
            Inst.box env.mt Ilpredef.int32_ref ;
            Inst.stelem_ref env.mt
      )

  | Tprim (CPnewprim tp,tl) -> 
      begin match tp with
	| TP_buildobject (cid,fullctorsig,flds) ->
	    Inst.newobj env.mt cid ".ctor" Il.Tvoid [];
	    ( fun v ->
		let write_field t fr =
		  Inst.ldloc_name env.mt v;
		  tlambda {env with wr=WRcont} t;
		  Inst.stfld_field env.mt fr Instance 
		in
		  List.iter2 write_field tl flds
	    )

	| TP_mktop cid -> 
	    Inst.newobj_class env.mt (find_class env cid.trnme) ;
	    (fun v -> ())

	| TP_mkenv cid -> 
	    let rec l1_iter2 f l1 l2 = (* iteration is based on l1 *)
	      match (l1, l2) with
		  ([], _) -> ()
		| (a1::l1, a2::l2) -> f a1 a2; l1_iter2 f l1 l2
		| (_, _) -> invalid_arg "l1_iter2"
	    in
	    if tl = [] then begin
	       Inst.newobj_class env.mt (find_class env cid.trnme) ;
	    (fun v -> ()) end
	    else begin
	    let cl = find_class env cid.trnme in
	    let ctormet = 
	      try List.find (fun x -> x.mnme=".ctor") cl.tdmet 
	      with Not_found -> Utils.bug "Delayed TP_mkenv" cl.tdnme
	    in
	    let fields = cl.tdfld in
	      for i = 1 to (List.length tl) do Inst.ldnull env.mt done; 
	      Inst.newobj_class env.mt cl;
		( fun  v -> 
		    l1_iter2 (fun t fd -> 
				  Inst.ldloc_name env.mt v;
				  tlambda {env with wr=WRcont} t;
				  Inst.stfld_field env.mt (Ilbuild.fieldref_of_fielddef cl fd) (if List.mem FAstatic fd.fatt then NoInstance else Instance)
			       ) tl fields; )
	    end

	| TP_cast (x,y) ->
	    (* CAUTION: the following code is correct iif the cast instruction does not copy objects *)
	    begin match tl with 
		[t] -> 
		  let res = delayed_tlambda {env with wr=WRcont} t in
		    rcast {env with wr=WRcont} x y Tnop;
		    (fun v -> 
		       Inst.ldloc_name env.mt v;
		       let iltx = Ilm.to_il x in
		       let v2=Ilbuild.new_register env.mt iltx in
			 rcast {env with wr=WRcont} y x Tnop; 
			 Inst.stloc_name env.mt v2;
			 res v2;
			 Ilbuild.release_loc env.mt v2
		    ) 
	      | _ -> Utils.bug "Compil.delayed_tlambda" "Wrong TP_cast"
	    end

	| TP_mkrec _ -> Utils.bug "Compil.delayed_tlambda" "TnewPrim:TP_mkrec"
	| _ -> Utils.bug "Compil.delayed_tlambda" "TnewPrim:?"
      end
  | Tprim (_,_) -> Utils.bug "Compil.delayed_tlambda" "TPrim"
  | Tassign (_,_) -> Utils.bug "Compil.delayed_tlambda" "Tassign"
  | Tsend (_,_,_) -> Utils.bug "Compil.delayed_tlambda" "Tsend"
  | Ttrywith (_,_,_) -> Utils.bug "Compil.delayed_tlambda" "Trywith"

  | _ -> Utils.bug "Compil.delayed_tlambda" "not implemented" 
	
and switch env indexes actions swct = 
  let br_end = Ilbuild.new_label env.mt in
  let env2 = if env.wr=WRcont then {env with wr=WRbr br_end} else env in
  let len_cb = Array.length actions in 
  let len_ib = Array.length indexes in 
  let lbl_tab = Array.init len_cb (fun x -> Ilbuild.new_label env2.mt) in 
  let lbl_lst = ref [] in 
    for i = len_ib - 1 downto 0 do 
      lbl_lst := lbl_tab.(indexes.(i)) :: !lbl_lst
    done ;
    (* find the right way to grab the discriminating integer *)
    if not (Ilm.is_immediate swct) then begin
      if not (Ilm.is_variant swct) then Inst.call_method env2.mt Ilpredef.boxint_tag (* polymorphic variants, actually *)
      else Inst.ldfld_field env2.mt Ilpredef.variant_tag Instance
    end;

    let br_lst = Inst.switch env2.mt !lbl_lst in
      for i = 0 to len_cb - 1 do 
	let lbl = lbl_tab.(i) in 
	let br = List.find (fun (x,_) -> x=lbl) br_lst in
	  Inst.label_force env2.mt br ;
	  tlambda env2 actions.(i) 
      done  ;
      if env.wr=WRcont then Inst.label_follow env2.mt br_end 
      
                         (** *************** **)

let objt = Ilpredef.object_type 
let objv = (objt,None)

let setup_func_skeleton env fd = 
  let il = Utils.someof fd.tfd_id.ilinfo in
  let cl = Ilbuild.new_class env.icu il.ilns il.ilname in
    add_class env cl ;
    Ilbuild.add_catt cl CAserializable;
  let ctor = Ilbuild.new_ctor cl [] in
  let exec = Ilbuild.new_method cl "exec" il.ilrt il.ilsig in
    Ilbuild.add_matt exec MAstatic ;
    Inst.label_follow exec (Ilbuild.init_label exec);
  let apply = Ilbuild.new_method cl "apply" objt [objv] in 
    Ilbuild.add_matt apply MAvirtual ;
    (il,cl,ctor,exec,apply)

                         (** **************** **)
                         (**     TOPLEVEL     **)
                         (** **************** **)

(* toplevel function with one argument *)
let tfun_top_one env fd = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  (* class extends closure *)
    Ilbuild.add_extd cl Ilpredef.closure_ref ;
  (* constructor *)
    Inst.ldarg ctor 0 ;
    Inst.call_method ctor Ilpredef.closure_ctor ;
    Inst.ret ctor ;
  (* apply *)(* *)
  let targ = (List.hd fd.tfd_var).ltype in 
  if targ = CTvoid then ()
  else (Inst.ldarg apply 1 ;
	rcast {env with mt=apply; wr=WRcont} CTobject targ Tnop);
  ( match Ilm.is_immediate fd.tfd_rt with 
    true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
      rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
  | _  -> Inst.tail apply ;
      Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
  Inst.ret apply ;

  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe


(* toplevel function with curryfied arguments *)
let tfun_top_cur env fd = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  (* class extends closure *)
    Ilbuild.add_extd cl Ilpredef.closure_ref ;
  (* constructor *)
    Inst.ldarg ctor 0 ;
    Inst.call_method ctor Ilpredef.closure_ctor ;
    Inst.ret ctor ;
  (* apply *)
    for i=0 to (List.length fd.tfd_var)-1 do 
      let x = List.nth fd.tfd_var i in
      Inst.ldarg apply 1 ;
      Inst.castclass_spec apply Ilpredef.objtab_type ;
      Inst.ldci4 apply i ;
      Inst.ldelem_ref apply ;
      rcast {env with mt=apply; wr=WRcont} CTobject x.ltype Tnop
    done ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe

(* general toplevel function *)
let tfun_top env fd = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  let args,arg = last_list fd.tfd_var "ll1" in
  let sizeenv = List.length args in
  (* class extends PappClosure *)
    Ilbuild.add_extd cl Ilpredef.clospapp_ref ;
  (* constructor *)
    Inst.ldarg ctor 0 ;
    Inst.ldci4 ctor sizeenv ;
    Inst.call_method ctor Ilpredef.clospapp_ctor ;
    Inst.ret ctor ;
  (* apply *)
    (*-- if self.cur==SIZE then FINAL_CALL *)
    Inst.ldarg apply 0 ;
    Inst.ldfld_field apply Ilpredef.clospapp_cur Instance;
    Inst.ldci4 apply sizeenv ;
    let final_call = Inst.beq apply "FINAL_CALL" in 
    (*-- return Env.Compil::gen_apply(self , new FUN() , argt ) *)
    Inst.ldarg apply 0 ;
    Inst.newobj_class apply cl ;
    Inst.ldarg apply 1 ; 
    Inst.tail apply ;
    Inst.call_method apply Ilpredef.clospapp_genapply ;
    Inst.ret apply ;
    (*-- FINAL_CALL : *)
    Inst.label apply final_call ;
    for i=0 to sizeenv-1 do 
      let x = List.nth args i in
      Inst.ldarg apply 0 ;
      Inst.ldfld_field apply Ilpredef.clospapp_envt Instance;
      Inst.ldci4 apply i ;
      Inst.ldelem_ref apply ;
      rcast {env with mt=apply; wr=WRcont} CTobject x.ltype Tnop
    done ;
    Inst.ldarg apply 1 ;
    rcast {env with mt=apply; wr=WRcont} CTobject arg.ltype Tnop ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe

                         (** ******************* **)
                         (**     ENVIRONMENT     **)
                         (** ******************* **)

(* envt function with one argument *)
let tfun_env_one env fd flds = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  (* class extends closure *)
    Ilbuild.add_extd cl Ilpredef.closure_ref ;
  (* fields *)
    let fields = 
      let dec_field x = Ilbuild.new_field cl (snd x.did) (Ilm.to_il x.dtype) in
      List.map dec_field flds in
  (* constructor *)
    let ctor_sig = List.map (fun x -> x.fsig , Some x.fnme) fields in 
    ctor.mprm <- ctor_sig ;
    Inst.ldarg ctor 0 ;
    Inst.call_method ctor Ilpredef.closure_ctor ;
    for i = 1 to List.length fields do 
      Inst.ldarg ctor 0 ;
      Inst.ldarg ctor i ;
      Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl (List.nth fields (i-1))) Instance
    done ;
    Inst.ret ctor ;
  (* apply *)
    Inst.ldarg apply 1 ;
    let targ = (List.hd fd.tfd_var).ltype in 
    rcast {env with mt=apply; wr=WRcont} CTobject targ Tnop;
    Inst.ldarg apply 0 ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe


(* envt function with curryfied arguments *)
let tfun_env_cur env fd flds = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  (* class extends closure *)
    Ilbuild.add_extd cl Ilpredef.closure_ref ;
  (* fields *)
    let fields = 
      let dec_field x = Ilbuild.new_field cl (snd x.did) (Ilm.to_il x.dtype) in
      List.map dec_field flds in

  (* constructor *)
    let ctor_sig = List.map (fun x -> x.fsig , Some x.fnme) fields in 
    ctor.mprm <- ctor_sig ;
    Inst.ldarg ctor 0 ;
    Inst.call_method ctor Ilpredef.closure_ctor ;
    for i = 1 to List.length fields do 
      Inst.ldarg ctor 0 ;
      Inst.ldarg ctor i ;
      Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl (List.nth fields (i-1))) Instance
    done ;
    Inst.ret ctor ;
  (* apply *)
    for i=0 to (List.length fd.tfd_var)-2 do (* do not iterate until argenv *) 
      let x = List.nth fd.tfd_var i in
      Inst.ldarg apply 1 ;
      Inst.castclass_spec apply Ilpredef.objtab_type ;
      Inst.ldci4 apply i ;
      Inst.ldelem_ref apply ;
      rcast {env with mt=apply; wr=WRcont} CTobject x.ltype Tnop
    done ;
    Inst.ldarg apply 0 ; 
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe

(* general envt function *)
let tfun_env env fd flds = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  let args,arg =    
    let vars,_ = last_list fd.tfd_var "error ll2" in 
      last_list vars  cl.tdnme in
  let sizeenv = List.length args in
  (* class extends PappClosure *)
    Ilbuild.add_extd cl Ilpredef.clospapp_ref ;
  (* fields *)
    let fields = 
      let dec_field x = Ilbuild.new_field cl (snd x.did) (Ilm.to_il x.dtype) in
      List.map dec_field flds in
    let self = Ilbuild.new_field cl "self" (Tclass (Ilbuild.typeref_of_typedef cl)) in
  (* constructor *)
    let ctor_sig = List.map (fun x -> x.fsig , Some x.fnme) fields in 
    ctor.mprm <- ctor_sig ;
    Inst.ldarg ctor 0 ;
    Inst.ldci4 ctor sizeenv ;
    Inst.call_method ctor Ilpredef.clospapp_ctor ;
    for i = 1 to List.length fields do 
      Inst.ldarg ctor 0 ;
      Inst.ldarg ctor i ;
      Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl (List.nth fields (i-1))) Instance
    done ;
    Inst.ldarg ctor 0 ;
    Inst.ldarg ctor 0 ;
    Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl self) Instance;
    Inst.ret ctor ;
  (* apply *)
    (*-- if self.cur==SIZE then FINAL_CALL *)
    Inst.ldarg apply 0 ;
    Inst.ldfld_field apply Ilpredef.clospapp_cur Instance;
    Inst.ldci4 apply sizeenv ;
    let final_call = Inst.beq apply "FINAL_CALL" in 
    (*-- return Env.Compil::gen_apply(self , .... , argt ) *)
    Inst.ldarg apply 0 ;
      (*-- new FUN (fd_1..fd_n) *)
      List.iter (fun x-> Inst.ldarg apply 0; Inst.ldfld_field apply (Ilbuild.fieldref_of_fielddef cl x) Instance) fields ;
      Inst.newobj_class apply cl ;
      (*-- Fun.self <- self.self *)
      Inst.dup apply ;
      Inst.ldarg apply 0;
      Inst.ldfld_field apply (Ilbuild.fieldref_of_fielddef cl self) Instance ; 
      Inst.stfld_field apply (Ilbuild.fieldref_of_fielddef cl self) Instance ;
      Inst.ldarg apply 1 ; 
      Inst.tail apply ;
      Inst.call_method apply Ilpredef.clospapp_genapply ;
      Inst.ret apply ;
      (*-- FINAL_CALL : *)
      Inst.label apply final_call ;
      for i=0 to sizeenv-1 do 
	let x = List.nth args i in
      Inst.ldarg apply 0 ;
      Inst.ldfld_field apply Ilpredef.clospapp_envt Instance;
      Inst.ldci4 apply i ;
      Inst.ldelem_ref apply ;
      rcast {env with mt=apply; wr=WRcont} CTobject x.ltype Tnop
    done ;
    Inst.ldarg apply 1 ;
    rcast {env with mt=apply; wr=WRcont} CTobject arg.ltype Tnop ;
    Inst.ldarg apply 0 ;
    Inst.ldfld_field apply (Ilbuild.fieldref_of_fielddef cl self) Instance ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe


                         (** ****************** **)
                         (**     RECURSIVE      **)
                         (** ****************** **)

(* recursive function with one argument *)
let tfun_rec_one env fd cid = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  let arg,argrec = match fd.tfd_var with 
      [x;y] -> x,y 
    | _ -> Utils.bug "Compil.tfun_rec_one" "not two args" in 
  (* class extends closure *)
    Ilbuild.add_extd cl Ilpredef.closure_ref ;
  (* fields *)
    let field = Ilbuild.new_field cl "rec" (Tclass cid) in 
  (* constructor *)
    ctor.mprm <- [field.fsig , Some field.fnme] ; 
    Inst.ldarg ctor 0 ;
    Inst.call_method ctor Ilpredef.closure_ctor ;
    Inst.ldarg ctor 0 ;
    Inst.ldarg ctor 1 ;
    Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl field) Instance ;
    Inst.ret ctor ;
  (* apply *)
    Inst.ldarg apply 1 ;
    let targ = arg.ltype in 
    rcast {env with mt=apply; wr=WRcont} CTobject targ Tnop;
    Inst.ldarg apply 0 ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe


(* recursive function with curryfied arguments *)
let tfun_rec_cur env fd cid = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  (* class extends closure *)
    Ilbuild.add_extd cl Ilpredef.closure_ref ;
  (* fields *)
    let field = Ilbuild.new_field cl "rec" (Tclass cid) in 
  (* constructor *)
    ctor.mprm <- [field.fsig , Some field.fnme] ; 
    Inst.ldarg ctor 0 ;
    Inst.call_method ctor Ilpredef.closure_ctor ;
    Inst.ldarg ctor 0 ;
    Inst.ldarg ctor 1 ;
    Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl field) Instance ;
    Inst.ret ctor ;
  (* apply *)
    for i=0 to (List.length fd.tfd_var)-2 do 
      let x = List.nth fd.tfd_var i in
      Inst.ldarg apply 1 ;
      Inst.castclass_spec apply Ilpredef.objtab_type ;
      Inst.ldci4 apply i ;
      Inst.ldelem_ref apply ;
      rcast {env with mt=apply; wr=WRcont} CTobject x.ltype Tnop
    done ;
    Inst.ldarg apply 0 ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe


(* general recursive function *)
let tfun_rec env fd cid = 
  let (il,cl,ctor,exec,apply) = setup_func_skeleton env fd in 
  let vars,argrec = last_list fd.tfd_var "ll4" in
  let args,arg = last_list vars "ll5" in
  let sizeenv = List.length args in
  (* class extends PappClosure *)
    Ilbuild.add_extd cl Ilpredef.clospapp_ref ;
  (* fields *)
    let field = Ilbuild.new_field cl "rec" (Tclass cid) in 
  (* constructor *)
    ctor.mprm <- [field.fsig , Some field.fnme] ;
    Inst.ldarg ctor 0 ;
    Inst.ldci4 ctor sizeenv ;
    Inst.call_method ctor Ilpredef.clospapp_ctor ;
    Inst.ldarg ctor 0 ;
    Inst.ldarg ctor 1 ;
    Inst.stfld_field ctor (Ilbuild.fieldref_of_fielddef cl field) Instance ;
    Inst.ret ctor ;
  (* apply *)
    (*-- if self.cur==SIZE then FINAL_CALL *)
    Inst.ldarg apply 0 ;
    Inst.ldfld_field apply Ilpredef.clospapp_cur Instance ;
    Inst.ldci4 apply sizeenv ;
    let final_call = Inst.beq apply "FINAL_CALL" in 
    (*-- return Env.Compil::gen_apply(self , .... , argt ) *)
    Inst.ldarg apply 0 ;
      (*-- new FUN (fd) *)
      Inst.ldarg apply 0; 
      Inst.ldfld_field apply (Ilbuild.fieldref_of_fielddef cl field) Instance ;
      Inst.newobj_class apply cl ;
    Inst.ldarg apply 1 ; 
    Inst.tail apply ;
    Inst.call_method apply Ilpredef.clospapp_genapply ;
    Inst.ret apply ;
    (*-- FINAL_CALL : *)
    Inst.label apply final_call ;
    for i=0 to sizeenv-1 do 
      let x = List.nth args i in
      Inst.ldarg apply 0 ;
      Inst.ldfld_field apply Ilpredef.clospapp_envt Instance ;
      Inst.ldci4 apply i ;
      Inst.ldelem_ref apply ;
      rcast {env with mt=apply; wr=WRcont} CTobject x.ltype Tnop
    done ;
    Inst.ldarg apply 1 ;
    rcast {env with mt=apply; wr=WRcont} CTobject arg.ltype Tnop ;
    Inst.ldarg apply 0 ;
    ( match Ilm.is_immediate fd.tfd_rt with 
        true -> Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ;
                rcast {env with mt=apply; wr=WRcont} fd.tfd_rt CTobject Tnop
        | _  -> Inst.tail apply ;
                Inst.call_method apply (Ilbuild.methodref_of_methode cl exec) ) ;
    Inst.ret apply ;
  (* exec *)
  fun () -> 
    tlambda {env with mt=exec} fd.tfd_exe



let tfundec env fd = 
  match fd.tfd_sts with 
      TFStop when List.length fd.tfd_var = 1 -> tfun_top_one env fd 
    | TFStop when fd.tfd_cur -> tfun_top_cur env fd 
    | TFStop -> tfun_top env fd 

    | TFSenv lfld when List.length fd.tfd_var = 2-> tfun_env_one env fd lfld  
    | TFSenv lfld when fd.tfd_cur -> tfun_env_cur env fd lfld
    | TFSenv lfld -> tfun_env env fd lfld

    | TFSrec cid when List.length fd.tfd_var = 2 -> tfun_rec_one env fd cid
    | TFSrec cid when fd.tfd_cur ->  tfun_rec_cur env fd cid
    | TFSrec cid -> tfun_rec env fd cid


                         (** ************************************ **)
                         (**      CLOSURE  AND ALGEBRAIC TYPES    **)
                         (** ************************************ **)

let toplevel_deftypes_listener_hook = ref (fun name -> (): string -> unit) (* TODO ??? REMOVE !!  ? *)

let tclassdec env cd = 
  let cl = Ilbuild.new_class env.icu cd.tcd_id.trnsp cd.tcd_id.trnme in
  let prt_ctor = match cd.tcd_kind with 
      CKsharedclosure -> Ilpredef.object_ctor
    | CKvariant _ -> Ilbuild.add_extd cl Ilpredef.variant_ref;Ilpredef.variant_ctor
    | CKrecord -> Ilbuild.add_extd cl Ilpredef.record_ref;Ilpredef.record_ctor
  in
    add_class env cl ;
    let fl = List.map (fun v -> Ilbuild.new_field cl v.lid (Ilm.to_il v.ltype)) cd.tcd_fld in
      (* register for toplevel *)
      begin match cd.tcd_kind with
	  CKvariant _ | CKrecord -> !toplevel_deftypes_listener_hook cl.tdnme
	| _ -> ()
      end;
	
    
    (* the first, trivial ctor is useful for delayed structures *)
    let simplector = Ilbuild.new_ctor cl [] in 
      Inst.ldarg simplector 0 ;
      begin match cd.tcd_kind with CKvariant tag -> Inst.ldci4 simplector tag | _ -> () end;
      Inst.call_method simplector prt_ctor ;
      (* *)
      begin match cd.tcd_kind with 
	  CKvariant _ ->
	    Inst.ldarg simplector 0;
	    Inst.ldci4 simplector 0;
	    Inst.stfld simplector Ilpredef.variant_size
	| _ -> ()
      end;
      (* *)
      Inst.ret simplector ;
      (* only build it if it is different from simplector ... *)
      let ctor = 
	if List.length fl > 0 then begin
	  let ctor = Ilbuild.new_ctor cl (List.map (fun fld -> (fld.fsig,if cd.tcd_kind = CKrecord then Some fld.fnme else None)) fl) in       
	    Inst.ldarg ctor 0 ;
	    begin match cd.tcd_kind with CKvariant tag -> Inst.ldci4 ctor tag | _ -> () end;
	    Inst.call_method ctor prt_ctor ;
	    (* *)
	    begin match cd.tcd_kind with 
		CKvariant _ ->
		  Inst.ldarg ctor 0;
		  Inst.ldci4 ctor (List.length fl);
		  Inst.stfld ctor Ilpredef.variant_size
	      | _ -> ()
	    end;
	    (* *)
	    let argcpt = ref 0 in
	    let initfld fld =
	      incr argcpt;
	      Inst.ldarg ctor 0;
	      Inst.ldarg ctor !argcpt;
	      Inst.stfld ctor (Ilbuild.fieldref_of_fielddef cl fld)
	    in 
	      List.iter initfld fl;
	      Inst.ret ctor;
	    ctor
	end 
	else simplector 
      in
      let build_comparison_method typ =
	let compare = Ilbuild.new_method cl "compare" Tint32 [(typ,None)] in
	  Ilbuild.add_matt compare MAvirtual ;
	  if List.length fl = 0 then
	    begin
	      Inst.ldci4 compare 0;
	      Inst.ret compare
	    end
	  else begin
	    let lbl_zero = Ilbuild.new_label compare in
	      begin match cd.tcd_kind with 
		  CKrecord -> 
		    (* not necessary for variants, because tested in CamIl.Variant generic method *)
		    Inst.ldarg compare 0;
		    Inst.ldarg compare 1;
		    ignore (Inst.beq compare lbl_zero)
		| _ -> ()
	      end;
	      Inst.ldarg compare 1;
	      let clref = Ilbuild.typeref_of_typedef cl in
		Inst.castclass compare clref;
		let tocomp = "tocomp" in
		  Ilbuild.create_local compare tocomp (Tclass clref);
		  Inst.stloc_name compare tocomp;
		  let result = "res" in
		    Ilbuild.create_local compare result Tint32;
		    let necessary_locals = ref [] 
		    and local_cpt = ref 0 in
		    let get_locals iltype =
		      try List.assoc iltype !necessary_locals
		      with Not_found ->
			let newsuffix = incr local_cpt;string_of_int !local_cpt in
			let newloc1 = "a"^newsuffix and newloc2 = "b"^newsuffix in
			  necessary_locals := (iltype,(newloc1,newloc2))::(!necessary_locals);
			  Ilbuild.create_local compare newloc1 iltype;
			  Ilbuild.create_local compare newloc2 iltype;
			  (newloc1,newloc2) in
		    let lbl_one = Ilbuild.new_label compare in
		    let counter = ref 0 in
		    let cmp_field fld tfld =
		      incr counter;
		      let lbl_next = Ilbuild.new_label compare in
		      let fref = Ilbuild.fieldref_of_fielddef cl fld in
			(* the following instructions depends on the type of the field *)  
			(* we match against the possible implementation types of fields *)
			(* i.e values that might be returned by Ilm.ctstype_of_typeinfo *)
			begin
			  match fld.fsig with
			      (* value types *)
			    | (Tint32 | Tint64 | Tnint | Tbool | Tchar | Tfloat64) -> 
				let (loc1,loc2) = get_locals fld.fsig in
				  Inst.ldarg compare 0;
				  Inst.ldfld compare fref;
				  Inst.dup compare;
				  Inst.stloc_name compare loc1;
				  Inst.ldloc_name compare tocomp;
				  Inst.ldfld compare fref;
				  Inst.dup compare;
				  Inst.stloc_name compare loc2;
				  ignore (Inst.beq compare lbl_next);
				  Inst.ldloc_name compare loc1;
				  Inst.ldloc_name compare loc2;
				  ignore (Inst.bgt compare lbl_one);
				  Inst.ldci4 compare (-1);
				  Inst.ret compare;
				  Inst.label_follow compare lbl_next;
				  if !counter = List.length fl then begin
				    Inst.ldci4 compare 0;
				    Inst.ret compare
				  end
				    (* others should be class types *)
			    | _ ->  
				Inst.ldarg compare 0;
				Inst.ldfld compare fref;
				Inst.ldloc_name compare tocomp;
				Inst.ldfld compare fref;
				let cmpmeth = Ilm.get_comparison_method tfld.ltype in
				  if cmpmeth.mrccn = Instance then Inst.callvirt_method compare cmpmeth
				  else Inst.call_method compare cmpmeth;
				  if !counter < List.length fl then begin
				    Inst.dup compare;
				    Inst.stloc_name compare result;
				    ignore (Inst.brfalse compare lbl_next);
				    Inst.ldloc_name compare result;
				    Inst.ret compare;
				    Inst.label_follow compare lbl_next
				  end 
				  else Inst.ret compare
			end
		    in
		      List.iter2 cmp_field fl cd.tcd_fld;
		      Inst.label_follow compare lbl_one;
		      Inst.ldci4 compare 1;
		      Inst.ret compare;
		      Inst.label_follow compare lbl_zero;
		      Inst.ldci4 compare 0;
		      Inst.ret compare
	  end
      in
      let build_hashcode_method typ =
	let hashcode = Ilbuild.new_method cl "hashcode" Tint32 [] in
	  Ilbuild.add_matt hashcode MAvirtual ;
	  if List.length fl = 0 then
	    begin
	      Inst.ldci4 hashcode 0;
	      Inst.ret hashcode
	    end
	  else begin
	    let hash_field fld tfld =
	      Inst.ldci4 hashcode 19;
	      Inst.mul hashcode;
	      let fref = Ilbuild.fieldref_of_fielddef cl fld in
		(* the following instructions depends on the type of the field *)  
		(* we match against the possible implementation types of fields *)
		(* i.e values that might be returned by Ilm.ctstype_of_typeinfo *)
		begin
		  match fld.fsig with
		      (* value types *)
		    | (Tint32 | Tint64 | Tnint | Tbool | Tchar) ->
			Inst.ldarg hashcode 0;
			Inst.ldfld hashcode fref;
			Inst.convi4 hashcode
		    | Tfloat64 -> (* !! ldsflda ... *)
			(* pas tout a fait exact ... *)
			Inst.ldarg hashcode 0;
			Inst.ldfld hashcode fref;
			Inst.convi4 hashcode
			  (* others should be class types *)
		    | _ ->  
			Inst.ldarg hashcode 0;
			Inst.ldfld hashcode fref;
			let hashmeth = Ilm.get_hashcode_method tfld.ltype in 
			  if hashmeth.mrccn = Instance then Inst.callvirt_method hashcode hashmeth 
			  else Inst.call_method hashcode hashmeth; 
		end;
		Inst.add hashcode
	    in
	      Inst.ldci4 hashcode 0;		
	      List.iter2 hash_field fl cd.tcd_fld;
	      Inst.call_method hashcode Ilpredef.math_abs;
	      Inst.ret hashcode
	  end
      in
      let build_equality_method typ =
	let equals = Ilbuild.new_method cl "equals" Tbool [(typ,None)] in
	  Ilbuild.add_matt equals MAvirtual ;
	  if List.length fl = 0 then
	    begin
	      Inst.ldci4 equals 1;
	      Inst.ret equals
	    end
	  else begin
	   let lbl_one = Ilbuild.new_label equals in
	    begin match cd.tcd_kind with 
		CKrecord -> 
		  (* not necessary for variants, because tested in CamIl.Variant generic method *)
		  Inst.ldarg equals 0;
		  Inst.ldarg equals 1;
		  ignore (Inst.beq equals lbl_one)
	      | _ -> ()
	    end;
	  Inst.ldarg equals 1;
	  let clref = Ilbuild.typeref_of_typedef cl in
	    Inst.castclass equals clref;
	    let tocomp = "tocomp" in
	      Ilbuild.create_local equals tocomp (Tclass clref);
	      Inst.stloc_name equals tocomp;
	      let lbl_false = Ilbuild.new_label equals in
		
	      let cmp_field fld tfld =
		let fref = Ilbuild.fieldref_of_fielddef cl fld in
		  Inst.ldarg equals 0;
		  Inst.ldfld equals fref;
		  Inst.ldloc_name equals tocomp;
		  Inst.ldfld equals fref;
		  (* the following instructions depends on the type of the field *)	  
		  (* we match against the possible implementation types of fields *)
		  (* i.e values that might be returned by Ilm.ctstype_of_typeinfo *)
		  begin
		    match fld.fsig with
			(* value types *)
		      | (Tint32 | Tint64 | Tnint | Tfloat64 ) -> Inst.ceq equals
		      | (Tbool | Tchar ) -> Inst.ceq equals
			  (* others should be class types *)
		      | _ ->  
			  let eqmeth = Ilm.get_equal_method tfld.ltype in
			    if eqmeth.mrccn = Instance then Inst.callvirt_method equals eqmeth
			    else Inst.call_method equals eqmeth;
		  end;
		  ignore (Inst.brfalse equals lbl_false);
	      in 
		List.iter2 cmp_field fl cd.tcd_fld;
		(* for best speed, compare valuetypes first !! *)
		Inst.ldci4 equals 1;
		Inst.ret equals;
		Inst.label_follow equals lbl_false;
		Inst.ldci4 equals 0;
		Inst.ret equals;
		Inst.label_follow equals lbl_one;
		Inst.ldci4 equals 1;
		Inst.ret equals
	  end
      in
      let build_duplication_method typ =
	let clref = Ilbuild.typeref_of_typedef cl in
	let duplicate = Ilbuild.new_method cl "duplicate" typ [] in
	  Ilbuild.add_matt duplicate MAvirtual ;
	  if List.length fl = 0 then begin
	    Inst.ldarg duplicate 0;
	    Inst.ret duplicate
	  end 
	  else 
	    begin
	      let push_field fld = 
		let fref = Ilbuild.fieldref_of_fielddef cl fld in
		  Inst.ldarg duplicate 0;
		  Inst.ldfld duplicate fref
	      in
		List.iter push_field fl;
		Inst.newobj_ctor duplicate (Ilbuild.methodref_of_methode cl ctor);
		Inst.ret duplicate
	    end
      in
	match cd.tcd_kind with 
	    CKsharedclosure -> Ilbuild.add_catt cl CAserializable
	  | CKvariant _  -> begin
	      Ilbuild.add_catt cl CAserializable;
	      build_comparison_method Ilpredef.variant_type;
	      build_equality_method Ilpredef.variant_type;
	      build_duplication_method Ilpredef.variant_type;
	      build_hashcode_method Ilpredef.variant_type
	    end
	  | CKrecord -> begin
	      Ilbuild.add_catt cl CAserializable;
	      build_comparison_method Ilpredef.record_type;
	      build_equality_method Ilpredef.record_type;
	      build_duplication_method Ilpredef.record_type;
	      build_hashcode_method Ilpredef.record_type
	    end
	    

                         (** ************** **)
                         (**      UNIT      **)
                         (** ************** **)

(* classes definitions *)

let define_fields moduleimmersion name typexpr typenv =
  let tinfo = convert_typeannotation true typenv moduleimmersion typexpr in
  let tconcrete = Ilm.ctstype_of_typeinfo tinfo in
    {lid = name; ltype=tconcrete}
    
open Types
let define_algebraic_type kind moduleimmersion env name flds =
  let subnsp = List.fold_left (fun s md -> if s="" then md else md^"."^s) "" moduleimmersion in
  let nsid = if subnsp = "" then Naming.get_unit_id() else (Naming.get_unit_id())^"."^subnsp in
  let tref = Ilbuild.type_ref nsid name in
  let tfld = List.map (fun (fname,te) -> define_fields moduleimmersion fname te env) flds in
    { tcd_kind = kind ; tcd_id=tref; tcd_fld=tfld}

let define_type (moduleimmersion,(name,typekind,env)) =
  match typekind with 
      Type_record(flds,_) ->
	[define_algebraic_type CKrecord moduleimmersion env name (List.map (fun (fname,_,te) -> (fname,te)) flds)]
    | Type_variant cstrs -> 
	let rec create_fnames accu idx = function
	    [] -> List.rev accu
	  | te::rem -> create_fnames ((("x"^(string_of_int idx)),te)::accu) (idx+1) rem 
	in
	let rec define_cstrs accu tag = function
	    [] -> List.rev accu
	  | (cname,cargs)::rem -> 
	      let newaccu = 
		if not !Clflags.noctropt && (List.length cargs) = 0 then accu (* optimisation: don't need to define classes for constant contructors *)
		else (define_algebraic_type (CKvariant tag) moduleimmersion env (cname^"_OF_"^name) (create_fnames [] 0 cargs))::accu
	      in define_cstrs newaccu (tag+1) rem
	in
	  define_cstrs [] 0 cstrs
    | Type_abstract -> failwith "ILM.define_type abstract"

open Typemod
let define_classes () = 
  let rec harvest_tdtree tdt moduleimmersion accu =
    let accu2 = (List.map (fun (name,decl,env) -> (moduleimmersion,(name,decl.type_kind,env))) tdt.ctd_decls) @ accu in
      List.fold_left (fun acc tdt -> harvest_tdtree tdt (tdt.ctd_name::moduleimmersion) acc) accu2 tdt.ctd_children
  in
    List.fold_left (fun l x -> (define_type x)@l) [] (harvest_tdtree Typemod.camil_typedecls [] [])


let tintfdec prefixname icu =
  (* define classes for algebraic types in interface file *)
  let ns = String.capitalize(Filename.basename prefixname) in 
  let top = Ilbuild.new_class icu ns "Top" in
  let start = Ilbuild.new_method top "startup" Tvoid [] in 
    Ilbuild.add_matt start MAstatic; Ilbuild.add_matt start MApublic ;
    Ilbuild.add_miat start IAil; Ilbuild.add_miat start IAmanaged ;
    Inst.ret start;
    let env = env_init start icu in
      List.iter (tclassdec env) (define_classes())


let tunitdec ud icu = 
  let ns =  ud.tud_id in 
  let top = Ilbuild.new_class icu ns "Top" in
    
  let global = Ilbuild.new_field top "global" Ilpredef.objtab_type in
    Ilbuild.add_fatt global FAstatic ;
    
    let start = Ilbuild.new_method top "startup" Tvoid [] in 
      Ilbuild.add_matt start MAstatic; Ilbuild.add_matt start MApublic ;
      Ilbuild.add_miat start IAil; Ilbuild.add_miat start IAmanaged ;
      
      let env = env_init start icu in
	
	List.iter (tclassdec env) ud.tud_cd ;
	(* emit algebraic types classes definitions *)	
	if not !Clflags.rebuiltmode then List.iter (tclassdec env) (define_classes());

	let fd_del = List.map (tfundec env) ud.tud_fd in
	  List.iter (fun f -> f ()) fd_del ;
	  
	  Inst.ldci4 start (ud.tud_size+1) ;
	  Inst.newarr start Ilpredef.object_ref ;
	  Inst.stfld_field start (Ilbuild.fieldref_of_fielddef top global) NoInstance ;
	  tlambda env ud.tud_start

