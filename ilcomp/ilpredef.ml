(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

open Il

(* some of System API *)

let system_ns = "System"
let text_ns = "System.Text" 
let camil_ns = "CamIL" 

let object_ref = Ilbuild.type_ref system_ns "Object"
let object_type = Tobject
let object_ctor = Ilbuild.ctor_ref object_ref [] 
let objtab_type = Tvector object_type

let int8_type = Tint8
let int16_type = Tint16

let int32_ref = Ilbuild.type_ref system_ns "Int32"
let int32_boxed_type = Tclass int32_ref
let int32_type = Tint32
let int32_ctor = Ilbuild.ctor_ref int32_ref [Tint32,None]

let int64_ref = Ilbuild.type_ref system_ns "Int64"
let int64_boxed_type = Tclass int64_ref
let int64_type = Tint64
let int64_ctor = Ilbuild.ctor_ref int64_ref [Tint64,None]

let nint_ref = Ilbuild.type_ref system_ns "IntPtr"
let nint_boxed_type = Tclass nint_ref
let nint_type = Tnint 
let nint_ctor = Ilbuild.ctor_ref  nint_ref [Tnint,None]

let nint_ofint = Ilbuild.method_ref NoInstance nint_ref "op_Explicit" nint_type [Tint32,None]

let float64_ref = Ilbuild.type_ref system_ns "Double"
let float64_boxed_type = Tclass float64_ref
let float64_type = Tfloat64
let float64_ctor = Ilbuild.ctor_ref float64_ref [Tfloat64,None]

let string_ref = Ilbuild.type_ref system_ns  "String" 
let string_type = Tstring

let char_ref = Ilbuild.type_ref system_ns  "Char" 
let char_type = Tchar
let char_arr_type = Tvector char_type

let bool_ref = Ilbuild.type_ref system_ns  "Boolean" 
let bool_type = Tbool

let builder_ref = Ilbuild.type_ref text_ns "StringBuilder"
let builder_type = Tclass builder_ref 
let builder_ctor = Ilbuild.ctor_ref builder_ref [string_type,None] 
let builder_conv = Ilbuild.method_ref Instance builder_ref "ToString" string_type []
let builder_length = Ilbuild.method_ref Instance builder_ref "get_Length" Tint32 []
let builder_getchars = Ilbuild.method_ref Instance builder_ref "get_Chars" Tchar [Tint32,None]
let builder_setchars = Ilbuild.method_ref Instance builder_ref "set_Chars" Tvoid [Tint32,None;Tchar,None]

let string_ctor = Ilbuild.ctor_ref string_ref [char_arr_type,None]
let string_length = Ilbuild.method_ref Instance string_ref "get_Length" Tint32 []
let string_getchars = Ilbuild.method_ref Instance string_ref "get_Chars" Tchar [Tint32,None]
let string_to_chararray = Ilbuild.method_ref Instance string_ref "ToCharArray" char_arr_type []
let string_compare = Ilbuild.method_ref NoInstance string_ref "Compare" Tint32 [string_type,None;string_type,None]

let convert_ref = Ilbuild.type_ref system_ns "Convert"
let convert_type = Tclass convert_ref 
let charofint = Ilbuild.method_ref NoInstance convert_ref "ToChar" Tchar [Tint32,None]
let intofchar = Ilbuild.method_ref NoInstance convert_ref "ToInt32" Tint32 [Tchar,None]


let math_ref = Ilbuild.type_ref system_ns "Math"
let math_abs = Ilbuild.method_ref NoInstance math_ref "Abs" Tint32 [Tint32,None]


(* some of CamIL API *)

let boxint_ref_pvt =  Ilbuild.type_ref camil_ns "BoxInt"
let boxint_is = Ilbuild.method_ref NoInstance boxint_ref_pvt "is" Tint32 [object_type,None]
(*  let _ = Ilbuild.add_matt boxint_is MAstatic  *)
let boxint_tag = Ilbuild.method_ref NoInstance boxint_ref_pvt "tag" Tint32 [objtab_type,None]
(*  let _ = Ilbuild.add_matt boxint_tag MAstatic *)

let sysexn_ref =  Ilbuild.type_ref system_ns "Exception"
let sysexn_type = Tclass sysexn_ref 
let sysexn_ctor = Ilbuild.ctor_ref sysexn_ref [objtab_type,None] 

let exn_ref =  Ilbuild.type_ref camil_ns "Exception"
let exn_type = Tclass exn_ref 
let exn_field = Ilbuild.field_ref exn_ref "v1" objtab_type 
let exn_ctor = Ilbuild.ctor_ref exn_ref [objtab_type,None] 
let exn_raise = Ilbuild.method_ref NoInstance exn_ref "raise" Tvoid [objtab_type,None]
let exn_embed = Ilbuild.method_ref NoInstance  exn_ref "embedCLI" exn_type [sysexn_type,None]

let closure_ref = Ilbuild.type_ref camil_ns "Closure"
let closure_type = Tclass closure_ref 
let closure_ctor = Ilbuild.ctor_ref closure_ref [] 
let closure_apply = Ilbuild.method_ref Instance closure_ref "apply" object_type [object_type,None] 
(*  let _ = closure_apply.ccnv <- Instance ;
          closure_apply.matt <- [MAvirtual] *)

let clospapp_ref = Ilbuild.type_ref camil_ns "PappClosure"
let clospapp_type = Tclass clospapp_ref 
let clospapp_ctor = Ilbuild.ctor_ref clospapp_ref [Tint32,None] 
let clospapp_genapply = Ilbuild.method_ref NoInstance clospapp_ref "gen_apply" clospapp_type [clospapp_type,None;clospapp_type,None;object_type,None] 
(*  let _ = clospapp_genapply.ccnv <- Noinstance ;
          clospapp_genapply.matt <- [MAstatic] *)
let clospapp_cur = Ilbuild.field_ref clospapp_ref "nargs" Tint32 
let clospapp_envt = Ilbuild.field_ref clospapp_ref "args" objtab_type
let clospapp_copy =  Ilbuild.method_ref NoInstance clospapp_ref "copyenvt" Tvoid [clospapp_type,None;clospapp_type,None] 

let camil_hash_ref = Ilbuild.type_ref camil_ns "Hash"
let camil_hash_code = Ilbuild.method_ref NoInstance camil_hash_ref "HashCode" Tint32 [object_type,None]

let record_ref = Ilbuild.type_ref camil_ns "Record"
let record_type = Tclass record_ref 
let record_ctor = Ilbuild.ctor_ref record_ref []
let record_equals = Ilbuild.method_ref Instance record_ref "equals" Tbool [record_type,None] 
let record_compare = Ilbuild.method_ref Instance record_ref "compare" Tint32 [record_type,None]  
let record_hashcode = Ilbuild.method_ref Instance record_ref "hashcode" Tint32 []  


let variant_ref = Ilbuild.type_ref camil_ns "Variant"
let variant_type = Tclass variant_ref 
let variant_ctor = Ilbuild.ctor_ref variant_ref [(Tint32,Some "tag")]
let variant_tag = Ilbuild.field_ref variant_ref "tag" Tint32
let variant_size = Ilbuild.field_ref variant_ref "size" Tint32
let variant_equals = Ilbuild.method_ref Instance variant_ref "vequals" Tbool [variant_type,None] 
let variant_compare = Ilbuild.method_ref Instance variant_ref "vcompare" Tint32 [variant_type,None]  
let variant_hashcode = Ilbuild.method_ref Instance variant_ref "hashcode" Tint32 []  

let variant_optim_cc_ref = Ilbuild.type_ref camil_ns "Variant_OCC"



let camil_list_nil_ref = Ilbuild.type_ref camil_ns "Nil_OF_camil_list"
let camil_list_cons_ref = Ilbuild.type_ref camil_ns "Cons_OF_camil_list"
let camil_option_none_ref = Ilbuild.type_ref camil_ns "None_OF_camil_option"
let camil_option_some_ref = Ilbuild.type_ref camil_ns "Some_OF_camil_option"

let cst_ref = Ilbuild.type_ref camil_ns "Constant"
let cst_type = Tclass cst_ref 
let cst_failure = Ilbuild.field_ref cst_ref "Failure" objtab_type
(*  let _ = Ilbuild.add_fatt cst_failure FAstatic *)
let cst_assertfailure = Ilbuild.field_ref cst_ref "Assert_failure" objtab_type
(*  let _ = Ilbuild.add_fatt cst_assertfailure FAstatic *)
let cst_matchfail = Ilbuild.field_ref cst_ref "Match_failure" objtab_type
(*  let _ = Ilbuild.add_fatt cst_matchfail FAstatic  *)
let cst_invargt = Ilbuild.field_ref cst_ref "Invalid_argument" objtab_type
(*  let _ = Ilbuild.add_fatt cst_invargt FAstatic  *)
let cst_notfound = Ilbuild.field_ref cst_ref "Not_found" objtab_type
(*  let _ = Ilbuild.add_fatt cst_notfound FAstatic *)
let cst_endoffile = Ilbuild.field_ref cst_ref "End_of_file" objtab_type
(*  let _ = Ilbuild.add_fatt cst_endoffile FAstatic *)
let cst_outofmemory = Ilbuild.field_ref cst_ref "Out_of_memory" objtab_type
(*  let _ = Ilbuild.add_fatt cst_outofmemory FAstatic *)
let cst_stackoverflow = Ilbuild.field_ref cst_ref "Stack_overflow" objtab_type
(* let _ = Ilbuild.add_fatt cst_stackoverflow FAstatic *)
let cst_syserror = Ilbuild.field_ref cst_ref "Sys_error" objtab_type
(*  let _ = Ilbuild.add_fatt cst_syserror FAstatic  *)
let cst_managedexception = Ilbuild.field_ref cst_ref "ManagedException" objtab_type
(*  let _ = Ilbuild.add_fatt cst_syserror FAstatic  *)

let comp_ref = Ilbuild.type_ref camil_ns "Compare"
let comp_type = Tclass comp_ref 
let comp_equals =  Ilbuild.method_ref NoInstance comp_ref "equal" Tbool [object_type,None;object_type,None] 
let comp_compare =  Ilbuild.method_ref NoInstance comp_ref "compare" Tint32 [object_type,None;object_type,None] 
let comp_lessthan =  Ilbuild.method_ref NoInstance comp_ref "lessthan" Tbool [object_type,None;object_type,None] 
let comp_greaterthan =  Ilbuild.method_ref NoInstance comp_ref "greaterthan" Tbool [object_type,None;object_type,None] 
let comp_lessequal =  Ilbuild.method_ref NoInstance comp_ref "lessequal" Tbool [object_type,None;object_type,None] 
let comp_greaterequal =  Ilbuild.method_ref NoInstance comp_ref "greaterequal" Tbool [object_type,None;object_type,None] 


(* TODO: rename as it is confusing ! cf builder & bldstr *)
let camilstring_ref = Ilbuild.type_ref camil_ns "String"
let camilstring_setsafe = Ilbuild.method_ref NoInstance camilstring_ref "set_safe" Tvoid [char_arr_type,None;Tint32,None;Tchar,None]
let camilstring_getsafe = Ilbuild.method_ref NoInstance camilstring_ref "get_safe" Tchar [char_arr_type,None;Tint32,None]
let string_of_object = Ilbuild.method_ref NoInstance camilstring_ref "string_of_object" string_type [object_type,None]
let bldstr_of_object = Ilbuild.method_ref NoInstance camilstring_ref "bldstr_of_object" builder_type [object_type,None]
let chararray_of_object = Ilbuild.method_ref NoInstance camilstring_ref "chararray_of_object" char_arr_type [object_type,None]
let string_setchars = Ilbuild.method_ref NoInstance camilstring_ref "string_set_error" Tvoid []


let array_ref = Ilbuild.type_ref camil_ns "Array"
let array_setsafe = Ilbuild.method_ref NoInstance array_ref "set_safe" Tvoid [objtab_type,None;Tint32,None;object_type,None]
let array_getsafe = Ilbuild.method_ref NoInstance array_ref "get_safe" object_type [objtab_type,None;Tint32,None]

let camil_obj_ref = Ilbuild.type_ref camil_ns "Obj"
let camil_obj_block = Ilbuild.method_ref NoInstance camil_obj_ref "block" objtab_type [Tint32,None;Tint32,None]
(* let _ = Ilbuild.add_matt camil_obj_block MAstatic *)


