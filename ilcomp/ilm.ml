(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

open Il
open Clambda (* namedtype *)
open Ctypedlambda (* HashedPath *)
open Format

let qualifiedtype_of_namedtype namedtype =
  let longname = List.fold_left (fun s md -> if s="" then md else md^"."^s) "" namedtype.nt_path in
    (* add current namespace prefix for internally defined types *)
    try 
      let clidx = String.rindex longname '.' in
      let current_unit_prefix = if !Clflags.toplevel_mode then "" else (Compilenv.current_unit_name())^"." in
	(if namedtype.nt_ext then "" else current_unit_prefix)^(String.sub longname 0 clidx) , String.sub longname (clidx+1) ((String.length longname) - (clidx+1))
    with Not_found -> (if namedtype.nt_ext then "" else (Compilenv.current_unit_name())),longname
      
let record_classes = (HashedPath.create 20 : typeref HashedPath.t)
let variant_classes = (HashedPath.create 20 : (typeref list) HashedPath.t)

let init_tables () = 
  init_algebtypes_table();
  HashedPath.clear record_classes;
  HashedPath.clear variant_classes;
  HashedPath.add variant_classes list_id [Ilpredef.camil_list_nil_ref;Ilpredef.camil_list_cons_ref];
  HashedPath.add variant_classes option_id [Ilpredef.camil_option_none_ref;Ilpredef.camil_option_some_ref]

let _ = init_tables ()

let record_class_server id = 
  if HashedPath.mem record_classes id then HashedPath.find record_classes id
  else 
    let nsp,nme = qualifiedtype_of_namedtype id in
    let tr = Ilbuild.type_ref nsp nme in
      HashedPath.add record_classes id tr;
      tr

let variant_class_server id tagoption = 
  try 
    match tagoption with
	Some tag ->
	  if HashedPath.mem variant_classes id then List.nth (HashedPath.find variant_classes id) tag
	  else 
	    let descr = match HashedPath.find algebtypes_table id with
		NTIrecord _ -> failwith "ILM.variant_class_server"
	      | NTIvariant desc -> desc in
	    let nsp,nme = qualifiedtype_of_namedtype id in
	    let trl = List.map (fun (cname,cargs) -> Ilbuild.type_ref nsp (cname^"_OF_"^nme)) descr in
	      HashedPath.add variant_classes id trl;
	      List.nth trl tag
      | None -> Ilpredef.variant_ref
    with  Failure "nth" -> failwith "ILM.variant_class_server:nth"	      


(* definition of concrete implementation type *)
type ctstype =
    CTvoid | CTobject
  | CTint | CTbint
  | CTint32 | CTbint32
  | CTint64 | CTbint64
  | CTnint | CTbnint
  | CTchar | CTbchar
  | CTfloat | CTbfloat
  | CTbool | CTbbool
  | CTstring | CTstrbld
  | CTexception
  | CTarray of ctstype 
  | CTlist of ctstype
  | CTrecord of namedtype
  | CTtuple of ctstype list
  | CTvariant of namedtype * int option
  | CTlazy of ctstype
  | CTclosure of Il.typeref | CTgenclosure | CTsharedclosure (* !! encore utile ? *)
  | CTclass of Il.typeref


let get_closure_repr = function 
    CTclosure ncid -> ncid
  | _ -> raise Not_found

let is_array = function CTarray _ -> true | _ -> false

let is_variant = function CTvariant _ -> true | _ -> false

let is_immediate = function
    CTvoid -> true (* !! ? *)
  | CTint | CTint32 | CTint64 | CTnint | CTchar | CTfloat | CTbool -> true
  | _ -> false

let get_comparison_method = function
    CTvariant _ -> Ilpredef.variant_compare
  | CTrecord _ -> Ilpredef.record_compare
  | _ -> Ilpredef.comp_compare

let get_equal_method = function
    CTvariant _ -> Ilpredef.variant_equals
  | CTrecord _ -> Ilpredef.record_equals
  | _ -> Ilpredef.comp_equals

let get_hashcode_method = function
    CTvariant _ -> Ilpredef.variant_hashcode
  | CTrecord _ -> Ilpredef.record_hashcode
  | _ -> Ilpredef.camil_hash_code



(* tells whether the representation is naturally a value type *)
(* CAUTION: this shall be constant throughout the alternative compilation modes *)
let is_immediate_typeinfo = function
  | TIint | TIchar | TIint32 | TIint64 | TInint |  TIbool 
  | TIfloat | TIvoid -> true
  | TIunit | TIstring -> false
  | TIvariant id -> 
      let cstrs = get_variant_description id in
	(List.filter (fun (_,params) -> params<>[])  cstrs)=[]  (* true iff only consts in sumtype *)
  | _ -> false

(* from type annotation to concrete type, to il CTS *)
(* this depends on the chosen compilation mode *)
let rec ctstype_of_typeinfo ti =  match ti with
    TIint -> CTint
  | TIchar -> CTchar 
  | TIint32 -> CTint (* TODO: merge CTint and CTint32 *)
  | TIint64 -> CTint64
  | TInint -> CTnint
  | TIfloat -> CTfloat
  | TIbool -> CTbool
  | TIunit -> CTobject
  | TIstring -> begin match !Clflags.stringrepr with 
      | Clflags.SRO_strbuilder -> CTstrbld
      | Clflags.SRO_string -> CTstring
      | Clflags.SRO_chararray -> CTarray CTchar
    end
  | TIexception -> CTarray CTobject
  | TIarray ti -> CTarray CTobject 
  | TIobject -> CTobject
  | TIarrow _ | TIgenclosure -> CTgenclosure
  | TIsharedclosure -> CTsharedclosure
  | TIvoid -> CTvoid
  | TIdontknow -> failwith "ILM.ctstype_of_typeinfo: TIdontknow"
  | TIpureIL il -> CTclass il
  | TInotimplemented st -> failwith ("ILM.ctstype_of_typeinfo: TInotimplemented "^st)

(*only in propag mode: 'when' clauses provide an appropriate filter, otherwise raise an error *)
  | TIlazy ti -> CTarray CTobject 
  | TIlist ti  -> if !Clflags.variantrepr_objarray then CTarray CTobject else CTvariant (list_id,None) 
  | TIoption ti -> if !Clflags.variantrepr_objarray then CTarray CTobject else CTvariant (option_id,None)
  | TIrecord tid -> if !Clflags.recordrepr_objarray then CTarray CTobject else CTrecord tid 
  | TIvariant tid -> 
      if is_immediate_typeinfo ti then CTint 
      else (if !Clflags.variantrepr_objarray then CTarray CTobject else CTvariant (tid,None))
  | TItuple til ->  CTarray CTobject

(*only in rebuild mode: 'when' clauses provide an appropriate filter, otherwise raise an error *)
  | TIblock -> CTarray CTobject  

let rec to_il = function
    CTvoid -> Tvoid
  | CTobject -> Tobject
  | CTint -> Tint32
  | CTbint -> Ilpredef.int32_boxed_type
  | CTint32  -> Tint32
  | CTbint32 -> Ilpredef.int32_boxed_type
  | CTint64  -> Tint64
  | CTbint64 -> Ilpredef.int64_boxed_type
  | CTnint -> Tnint
  | CTbnint -> Ilpredef.nint_boxed_type
  | CTchar -> Tchar
  | CTbchar -> assert false
  | CTfloat -> Tfloat64
  | CTbfloat -> Ilpredef.float64_boxed_type
  | CTbool -> Tbool
  | CTbbool -> assert false
  | CTstring -> Tstring
  | CTstrbld ->  Ilpredef.builder_type
  | CTexception -> Tvector (Tobject)
  | CTarray ct -> Tvector (to_il ct)
  | CTlist ct -> assert false
  | CTlazy ct -> assert false
  | CTrecord tid -> Tclass (record_class_server tid)
  | CTtuple ctl -> Tvector (Tobject)
  | CTvariant (tid,tag) -> Tclass (variant_class_server tid tag)
  | CTclosure il -> Tclass il
  | CTgenclosure -> Ilpredef.closure_type
  | CTsharedclosure -> Tobject (* should not happen ? *)
  | CTclass il -> Tclass il


let rec of_il = function 
    Tint32 -> CTint
  | Tint64 -> CTint64
  | Tnint -> CTnint
  | Tchar -> CTchar
  | Tbool -> CTbool
  | Tfloat64 -> CTfloat
  | Tvoid -> CTvoid
  | Tstring -> CTstring
(*  unit ... *)
  | Tclass tr when tr.trnsp = "System.Text" && tr.trnme ="StringBuilder" -> CTstrbld
  | Tclass tr when tr.trnsp = "CamIL" && tr.trnme ="Closure" -> CTgenclosure
  | Tobject -> CTobject
  | Tclass x -> CTclass x
  | Tvector x -> CTarray (of_il x)
  | _ -> failwith "ILM.of_il"


(* printing facilities *)
let nyb m s =
  let str = m.mnme ^ " - " ^ s in
    failwith ("ILM/YY not implemented : "^str)

open Il
let str_class x = x.trnsp ^ "." ^ x.trnme

let rec to_string = function
    CTint ->  "int"
  | CTchar ->  "char"
  | CTint32 ->  "int32"
  | CTint64 ->  "int64"
  | CTnint ->  "nint"
  | CTfloat ->  "float"
  | CTbool ->  "bool"
  | CTbint ->  "*int"
  | CTbchar ->  "*char"
  | CTbint32 ->  "*int32"
  | CTbint64 ->  "*int64"
  | CTbnint ->  "*nint"
  | CTbfloat ->  "*float"
  | CTbbool ->  "*bool"
  | CTstring ->  "string"
  | CTstrbld ->  "strbld"
  | CTexception ->  "exception"
  | CTarray ti ->  Printf.sprintf "%s[]" (to_string ti)
  | CTlazy ti ->  Printf.sprintf "%s lazy_t" (to_string ti)
  | CTlist ti ->  Printf.sprintf  "%s list" (to_string ti)
  | CTrecord id ->  (print_namedtype_path id)
  | CTtuple tilist ->  Printf.sprintf  "(%s)" (prlist "*" tilist)
  | CTvariant (id,None) -> (print_namedtype_path id)
  | CTvariant (id,Some tag) -> Printf.sprintf "%s[%d]" (print_namedtype_path id) tag
  | CTobject ->  "<obj>"
  | CTclosure cid -> Printf.sprintf "<C!%s>" (str_class cid) 
  | CTgenclosure -> "genclos"
  | CTsharedclosure -> "mclos" 
  | CTvoid ->  "void"
  | CTclass cid -> Printf.sprintf "<IL:%s>" (str_class cid)
and prlist sep = function
    [t] ->  Printf.sprintf "%s" (to_string t)
  | t::q -> Printf.sprintf "%s%s%s" (to_string t) sep (prlist sep q)
  | [] -> failwith "Printulambda.prlist"


let rec type_printer ppf = function
    CTint -> fprintf ppf "int"
  | CTchar -> fprintf ppf "char"
  | CTint32 -> fprintf ppf "int32"
  | CTint64 -> fprintf ppf "int64"
  | CTnint -> fprintf ppf "nint"
  | CTfloat -> fprintf ppf "float"
  | CTbool -> fprintf ppf "bool"
  | CTbint -> fprintf ppf "*int"
  | CTbchar -> fprintf ppf "*char"
  | CTbint32 -> fprintf ppf "*int32"
  | CTbint64 -> fprintf ppf "*int64"
  | CTbnint -> fprintf ppf "*nint"
  | CTbfloat -> fprintf ppf "*float"
  | CTbbool -> fprintf ppf "*bool"
  | CTstring -> fprintf ppf "string"
  | CTstrbld -> fprintf ppf "strbld"
  | CTexception -> fprintf ppf "exception"
  | CTarray ti ->  fprintf ppf "%a[]" type_printer ti
  | CTlazy ti ->  fprintf ppf "%a lazy_t" type_printer ti
  | CTlist ti ->  fprintf ppf "%a list" type_printer ti
  | CTrecord id ->  fprintf ppf "%s" (print_namedtype_path id)
  | CTtuple tilist ->  fprintf ppf  "(%a)" (prlist "*") tilist
  | CTvariant (id,None) -> fprintf ppf "%s" (print_namedtype_path id)
  | CTvariant (id,Some tag) -> fprintf ppf "%s[%d]" (print_namedtype_path id) tag
  | CTobject ->  fprintf ppf "<obj>"
  | CTclosure cid -> fprintf ppf "<C!%s>" (str_class cid) 
  | CTgenclosure -> fprintf ppf "genclos"
  | CTsharedclosure -> fprintf ppf "mclos" 
  | CTvoid ->  fprintf ppf "void"
  | CTclass cid -> fprintf ppf "<IL:%s>" (str_class cid)
and prlist sep ppf = function
    [t] ->  fprintf ppf "%a" type_printer t
  | t::q -> fprintf ppf "%a%s%a" type_printer t sep (prlist sep) q
  | [] -> ()

(* TODO: rename ; actually used in broader contexts than locals !! *)
type locl_tid = { lid: Il.id ; mutable ltype: ctstype }

(* TODO: implement it as typeref * locl_tid *)
type fild_tid = { did: Il.typeref * Il.id ; mutable dtype: ctstype }

type tconstant = 
    Tconst_int of int 
  | Tconst_char of char
  | Tconst_bool of bool
  | Tconst_float of string 
  | Tconst_bfloat of string
  | Tconst_string of ctstype * string 
  | Tconst_null


type newprim = 
  | TP_get_builtin of string
  | TP_get_field of fild_tid  
  | TP_set_field of fild_tid  
  | TP_get_global of Il.nsid
  | TP_pushint of int

  | TP_mktop of Il.typeref
  | TP_mkenv of Il.typeref
  | TP_mkrec of Il.typeref
  | TP_mkclos of Il.typeref

  | TP_buildobject of Il.typeref * Il.signature * (Il.fieldref list)

  | TP_raise of ctstype
  | TP_set_block of int
  | TP_get_block of int 
  | TP_string_length of ctstype
  | TP_string_set of ctstype * bool (* true means safe *)
  | TP_string_ref of ctstype * bool 
  | TP_stringcomp of Lambda.comparison (* all value comps *)
  | TP_eq 
  | TP_neq 
  | TP_cast of ctstype * ctstype 
  | TP_pushdummy of ctstype
  | TP_convint of Lambda.boxed_integer
  | TP_offsetref of int
   

type tprimitive = 
    CPlegacyprim of Lambda.primitive 
  | CPnewprim of newprim


type tlambda =
  | Tconst of tconstant
  | Tdirect_apply of Clambda.function_label * tlambda list
  | Tgeneric_apply of tlambda * tlambda list
  | Tlet of locl_tid * tlambda * tlambda
  | Tletrec of (locl_tid * tlambda) list * tlambda
  | Tprim of tprimitive * tlambda list
  | Tswitch of tlambda * ctstype * tlambda_switch
  | Tstaticfail of int * tlambda list
  | Tcatch of int * locl_tid list * tlambda * tlambda
  | Ttrywith of tlambda * locl_tid * tlambda
  | Tifthenelse of tlambda * tlambda * tlambda
  | Tsequence of tlambda * tlambda
  | Twhile of tlambda * tlambda
  | Tfor of locl_tid * tlambda * tlambda * Asttypes.direction_flag * tlambda
  | Tassign of locl_tid * tlambda
  | Tsend of tlambda * tlambda * tlambda list
  | Tifvar of tlambda * tlambda * tlambda
  | Tlocal of locl_tid
  | Targument of locl_tid 
  | Tnop

and tlambda_switch =
  { 
    ts_indexes: int array;
    ts_actions: tlambda array
  }


type tfunction_status = 
    TFStop      
  | TFSenv of fild_tid list 
  | TFSrec of Il.typeref

type tfundec = 
  { tfd_id : Clambda.function_label ;
    tfd_cur: bool ; 
    tfd_sts: tfunction_status ;
    tfd_var: locl_tid list ;
    tfd_exe: tlambda ;
    tfd_rt : ctstype }

(* type used to register classes other than CamIL.Closure ones *)
(* ie shared closures and algebraic types *)
type tclasskind =
    CKrecord 
  | CKvariant of int (* tag *)
  | CKsharedclosure

type tclassdec = { 
  tcd_kind : tclasskind ;
  tcd_id : Il.typeref ; 
  tcd_fld: locl_tid list }

type tunitdec =  
  { tud_id: Il.nsid ;
    tud_fd: tfundec list ;
    tud_cd: tclassdec list ;
    tud_size: int ;
    tud_start: tlambda
  }
