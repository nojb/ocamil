(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ildynamic.ml,v 1.36 2006/10/16 12:45:55 montela Exp $ *)

open Asmlink
open Ilcompile
open Ilpath
open Config
open Misc
open Il

let cpt = ref 0
let current_name () = "temptop" ^ (string_of_int !cpt)
let new_name () = incr cpt;current_name()
  
let last_assembly = ref (Obj.magic []:Obj.t)

let toplevel_deftypes_listener typname = 
  (* register the association *)
  let assblyref = {arver="0:0:0:0";arflg=false;arkey="";arnme="temptop"^(string_of_int (!cpt - 1));arhsh=""} in
  Ilcompile.toplevel_deftypes_association := (typname,assblyref)::(!Ilcompile.toplevel_deftypes_association)

let _ = Compil.toplevel_deftypes_listener_hook := toplevel_deftypes_listener

let write_MLTop_startupAll mltop_class =
  let startupAll = Ilbuild.new_method mltop_class "startupAll" Tvoid [] in
    Ilbuild.add_matt startupAll MApublic;
    Ilbuild.add_matt startupAll MAstatic;
    Ilbuild.add_miat startupAll IAmanaged;
    (* some initialisations *)
    write_MLTop_startupAll_setup_fields startupAll;
    let modtopref = Ilbuild.type_ref (String.capitalize (current_name())) "Top" in
      Inst.call_method startupAll (Ilbuild.method_ref NoInstance modtopref "startup" Tvoid []); 
      Inst.ret startupAll

let write_class_MLTop icu =
  let mltop_class = Ilbuild.new_class icu "" "MLTop" in
    write_MLTop_fields mltop_class;
    write_MLTop_startupAll mltop_class

let write_self_assembly icu exec_bname exec_exename_nodir =
  write_assembly_header icu exec_bname exec_exename_nodir;
  write_class_MLTop icu


let compile_phrase ppf (size,slam) =
  let icu  = {icuass=None;icuassref=[];icutypes=[]} in
  let modname = new_name() in
    Ilcompile.new_assemblyref (String.capitalize modname) "";
    let modid = Ident.create (String.capitalize modname) in
    let icu = compile_ilcompunit ppf icu (size,slam,modid) in
      (* like Ilcompile.link ... *)
      Config.load_path := "" :: List.rev_append !Clflags.include_dirs (Clflags.std_include_dir ()); (* ?? *)
      
      detect_missing_implementations(); (* ?? *)
      
      (* generation of ".assembly extern" *)
      write_external_assemblies_references icu;
      
      (* the assembly itself *)
      write_self_assembly icu modname modname; 
      Clflags.make_dll := true;
      let abuilder = 
	match !Clflags.compilation_mode with
	  | Clflags.PlainIL -> failwith "PlainIL mode is not supported for toplevel"
	  | Clflags.MarshalledIL -> link_resolve_references_toplevel icu 
      in
	last_assembly := abuilder


external camil_exec_toplevel_phrase_noarg : string -> string -> string -> unit = 
"void" "CamIL.Dynamic" "toplevel_phrase_noarg" "string" "string" "string"
		  
external camil_exec_toplevel_phrase_onearg : string -> string -> string -> Obj.t -> unit = 
"void" "CamIL.Dynamic" "toplevel_phrase_onearg" "string" "string" "string" "object"

external test_camil_exec_toplevel_phrase_noarg : Obj.t -> string -> string -> unit = 
"void" "CamIL.Dynamic" "test_toplevel_phrase_noarg" "class [mscorlib]System.Reflection.Assembly" "string" "string"
		  
external test_camil_exec_toplevel_phrase_onearg : Obj.t -> string -> string -> Obj.t -> unit = 
"void" "CamIL.Dynamic" "test_toplevel_phrase_onearg" "class [mscorlib]System.Reflection.Assembly" "string" "string" "object"


open Reflection

(*
let camil_exec_toplevel_phrase assemblypath classname methodname somearg = 
  let assembly = assembly__load_from assemblypath in
  let cli_class = assembly#get_type classname in
  let args = match somearg with
      None -> _new_cArray_cObject 0 
    | Some ob -> 
	let arg = _new_cArray_cObject 1 in
	  arg#set 0 (new _capsule_cObject (Obj.magic ob));
	  arg
  in
  let nullbinder = new _capsule_cBinder Jacare.null in
  let nullobject = new _capsule_cObject Jacare.null in
  let _ = cli_class#invoke_member methodname [BFInvokeMethod] nullbinder nullobject args in ()
										
let camil_exec_toplevel_phrase_noarg a c m = camil_exec_toplevel_phrase a c m None
let camil_exec_toplevel_phrase_onearg a c m o = camil_exec_toplevel_phrase a c m (Some o)
*)

let startup_dll dllname =
  let file_name = find_in_path !Config.load_path dllname in
    ignore (
      camil_exec_toplevel_phrase_noarg file_name "MLTop" "startup"
    )
  
    
let startup_dll_without_dependencies dllname =
  let file_name = find_in_path !Config.load_path dllname in
    ignore(
      camil_exec_toplevel_phrase_noarg file_name "MLTop" "startupAll"
    )

(*
let execute_last_phrase setvaluefun getvaluefun =
  let dllpath = tempdlldir^(current_name())^".dll" in
  let namespace = (String.capitalize (current_name())) ^ ".closures." in
    ignore(
      camil_exec_toplevel_phrase_noarg dllpath "MLTop"  "startupAll";
      camil_exec_toplevel_phrase_onearg dllpath (namespace^"__$definesetvalue") "exec" (Obj.repr setvaluefun);
      camil_exec_toplevel_phrase_onearg dllpath (namespace^"__$definegetvalue") "exec" (Obj.repr getvaluefun); 
      camil_exec_toplevel_phrase_onearg dllpath (namespace^"__$runme") "exec" (Obj.repr [])
    )
*)  

let execute_last_phrase setvaluefun getvaluefun =
  let namespace = (String.capitalize (current_name())) in
    ignore(
      test_camil_exec_toplevel_phrase_noarg !last_assembly (namespace^".Top") "startup"; 
      test_camil_exec_toplevel_phrase_onearg !last_assembly (namespace^".closures.__$definesetvalue") "exec" (Obj.repr setvaluefun);
      test_camil_exec_toplevel_phrase_onearg !last_assembly (namespace^".closures.__$definegetvalue") "exec" (Obj.repr getvaluefun); 
      test_camil_exec_toplevel_phrase_onearg !last_assembly (namespace^".closures.__$runme") "exec" (Obj.repr [])
    )

