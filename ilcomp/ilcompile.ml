(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ilcompile.ml,v 1.103 2007/03/24 02:40:05 montela Exp $ *)


open Format
open Misc
open Ilpath
open Utils
open Config
open Il
open Compil

let access_name ros = match ros with 
    None -> "a.out"
  | Some s -> s

let fl ppf () = pp_print_flush ppf ()
let sp ppf () = 
  for i=1 to 2 do pp_print_newline ppf () done ; fl ppf ()

let merge_icus newicu icu =
  icu.icutypes <- newicu.icutypes @ icu.icutypes;
  icu.icuassref <- newicu.icuassref @ icu.icuassref


let thisMLTop =
  let topref = Ilbuild.type_ref "" "MLTop" in
    topref.trscp <- ThisAssembly;
    topref


(*************** referencing external assemblies **************)

(* hashtable Modname * (input ordering * Assref) *)
let assemblyref_table = (Hashtbl.create 10 :(string,assemblyref) Hashtbl.t)
let assemblyresolve_table = (Hashtbl.create 100 :(string,(int*string)) Hashtbl.t)
let assref_counter = ref 0

let new_assemblyref modname assref =
  if !Clflags.verbose then Printf.printf "Adding assembly resolving hint (%s,%s)\n" modname assref;
  if  assref<>"" &&  not (Hashtbl.mem assemblyref_table assref)  then 
    (
       let token,version = Ilpath.get_token_version assref in
	 Hashtbl.add assemblyref_table assref {arver=version;arflg=false;arkey=token;arnme=assref;arhsh=""}
    );
  Hashtbl.add assemblyresolve_table modname (!assref_counter,assref);
  incr assref_counter

let toplevel_deftypes_association = ref [] (* holds pairs typename -> toplevel phrases assemblyref *)

let get_assemblyref modname =
    let _, arnme = Hashtbl.find assemblyresolve_table modname in
      if arnme = "" then ThisAssembly else ExternalAssembly (Hashtbl.find assemblyref_table arnme)

(* skips mscorlib and core_camil *)
let retrieve_nocore_refs () =
  let unordered =
    Hashtbl.fold (fun modname (i,assref) prevlist -> if (assref = "mscorlib") 
		    || (modname = "CamIL") 
		    || (assref = Naming.core_camil_assbly) 
		    || (List.exists (fun (i2,(modname2,assref2)) -> (modname,assref)=(modname2,assref2)) prevlist)
		  then prevlist else (i,(modname,assref))::prevlist) assemblyresolve_table []
  in List.map snd (List.sort (fun (i,_) (j,_) -> compare i j) unordered)

let retrieve_refs () =
  let unordered =
    Hashtbl.fold (fun modname (i,assref) prevlist -> if  (List.exists (fun (i2,(modname2,assref2)) -> (modname,assref)=(modname2,assref2)) prevlist)
		  then prevlist else (i,(modname,assref))::prevlist) assemblyresolve_table []
  in List.map snd (List.sort (fun (i,_) (j,_) -> compare i j) unordered)

let external_assemblies () = 
    Hashtbl.fold (fun modname (i,assref) prevlist -> if (assref = "" ) 
		    || (assref = "mscorlib") 
		    || (List.exists (fun assref2 -> assref=assref2) prevlist)
		    then prevlist else assref::prevlist) assemblyresolve_table []


let initialize_assemblyref () =
  Hashtbl.clear assemblyresolve_table;
  Hashtbl.clear assemblyref_table;
  assref_counter := 0;
  List.iter (fun libitem -> new_assemblyref 
	       (String.capitalize libitem)
	       (if !Clflags.compiling_camil_corelib then "" else Naming.core_camil_assbly))
    (List.filter (fun s -> s <> "pervasives") Ilpath.stdlib_items);
  new_assemblyref "Pervasives" (if !Clflags.nopervasives || !Clflags.compiling_camil_corelib then "" else Naming.core_camil_assbly);
  new_assemblyref "System" "mscorlib";
  new_assemblyref "CamIL" (if !Clflags.nopervasives || !Clflags.compiling_camil_corelib then "" else Naming.core_camil_assbly)

let resolve_all_references icu =
  let rec resolve_in_class cl =
    (match cl.tdext with None -> ()
       | Some typr -> resolve_in_typeref typr);
    List.iter resolve_in_typeref cl.tdimp;
    List.iter resolve_in_method cl.tdmet;
    List.iter resolve_in_field cl.tdfld
  and resolve_in_method met =
    resolve_in_signature met.locals;
    resolve_in_signature met.mprm;
    resolve_in_elementType met.mtyp;
    List.iter resolve_in_instruction met.minst
  and resolve_in_field fld =
    resolve_in_elementType fld.fsig
  and resolve_in_instruction inst =
    match inst with
      | Ibox typ -> resolve_in_TypeRefSpec typ
      | Icall mr -> resolve_in_methodref mr
      | Icallvirt mr -> resolve_in_methodref mr
      | Icastclass typ -> resolve_in_TypeRefSpec typ
      | Ildfld fr -> resolve_in_fieldref fr
      | Ildsfld fr -> resolve_in_fieldref fr
      | Inewarr typ -> resolve_in_TypeRefSpec typ
      | Inewobj mr -> resolve_in_methodref mr
      | Istfld fr -> resolve_in_fieldref fr
      | Istsfld fr -> resolve_in_fieldref fr
      | Iunbox typ -> resolve_in_TypeRefSpec typ
      | Iisinst typ -> resolve_in_TypeRefSpec typ
      | _ -> () (* Nothing to do for other instructions *)
  and resolve_in_methodref mr =
    resolve_in_elementType mr.mrtyp;
    resolve_in_signature mr.mrprm;
    resolve_in_typeref mr.mrcla
  and resolve_in_fieldref fr =
    resolve_in_typeref fr.frcla;
    resolve_in_elementType fr.frsig;
  and resolve_in_signature typl =
    List.iter (fun (typ,_)-> resolve_in_elementType typ) typl
  and resolve_in_TypeRefSpec = function
    | TypeRef tr -> resolve_in_typeref tr
    | TypeSpec typ -> resolve_in_elementType typ
  and resolve_in_elementType typ =
    match typ with
      | Tstar t -> resolve_in_elementType t
      | Tampersand t -> resolve_in_elementType t
      | Tvector t -> resolve_in_elementType t
      | Tvaluetype tr -> resolve_in_typeref tr
      | Tclass tr -> resolve_in_typeref tr
      | _ -> () (* Nothing to do for other types *)
  and resolve_in_typeref tr =
    let assbly,nsp0,nme0 =  (* explicit [assref] occur in external declarations *)
      try (* [assref] part of namespace ? *)
	let pos1 = String.index tr.trnsp '[' in
	let pos2 = String.index tr.trnsp ']' in
	let assref = String.sub tr.trnsp (pos1+1) (pos2-pos1-1) in
	  icu.icuassref <- (build_extref assref)::icu.icuassref;
	  build_extscope assref , String.sub tr.trnsp (pos2+1) ((String.length tr.trnsp)-pos2-1), tr.trnme
      with _ -> begin
	try (* [assref] part of classname ? *)
	  let pos1 = String.index tr.trnme '[' in
	  let pos2 = String.index tr.trnme ']' in
	  let assref = String.sub tr.trnme (pos1+1) (pos2-pos1-1) in
	    icu.icuassref <- (build_extref assref)::icu.icuassref;
	    build_extscope assref , tr.trnsp, String.sub tr.trnme (pos2+1) ((String.length tr.trnme)-pos2-1)
	with _ -> 
	  tr.trscp , tr.trnsp, tr.trnme
      end
    in
      if assbly = UnknownAssembly then
	begin
	  try
	    let nsp = 
	      try 
		let pos = String.index nsp0 '.' in
		  String.sub nsp0 0 pos
	      with Not_found -> nsp0 in
	      tr.trscp <- get_assemblyref nsp
	  with Not_found -> 
	    (* another chance for toplevel: link the referred type into a temptop*.dll assembly *)
	    begin try
	      let tempassref = List.assoc tr.trnme !toplevel_deftypes_association in
		tr.trscp <- ExternalAssembly tempassref
	    with Not_found -> failwith("Ilcompile: cannot resolve assembly ref. for "^tr.trnsp^","^tr.trnme)
	    end
	end
      else 
	begin
	  tr.trscp <- assbly;
	  tr.trnsp <- nsp0;
	  tr.trnme <- nme0
	end
  in
    List.iter resolve_in_class icu.icutypes
    


(*************** Compilation of a unit **************)


let compile_ilcompunit ppf icu (size,lam,modid) =
  let tu = Closure.intro size lam in
  let t = 
    let tu = 
      if !Clflags.rebuiltmode then Rebuildtypes.retype_ulambda (Ctypedlambda.to_ulambda tu) else tu in 
      if !Clflags.dump_ulambda then ( sp ppf (); Printulambda.ulambda false Printulambda.prtypeinfo ppf tu ; fl ppf ()) ;
      if !Clflags.dump_tulambda then ( sp ppf (); Printulambda.ulambda true Printulambda.prtypeinfo ppf tu ; fl ppf ()) ;
      Ilmcompile.comp_unit modid size tu in
    
    if !Clflags.dump_rtlambda then ( sp ppf (); Printilm.tunitdec ppf  t ; fl ppf ()) ;
    Compil.tunitdec t icu;
    if !Clflags.dump_il then 
      ( sp ppf () ; Emitil.init_co stdout ; Emitil.ilcompunit icu ; fl ppf  ()); 
    icu
    
let compile_implementation prefixname ppf (size,lam,modid) =
  let icu  = {icuass=None;icuassref=[];icutypes=[]} in
  let il = compile_ilcompunit ppf icu (size,lam,modid) in
    begin
      match !Clflags.compilation_mode with
	| Clflags.PlainIL ->     
	    begin
	      let fileil = open_out (prefixname ^ ".il") in 
		Emitil.init_co fileil ;
		Emitil.ilcompunit il ;
		close_out fileil 
	    end
	| Clflags.MarshalledIL -> Compilenv.set_appending_tocmx_fun (fun oc -> output_value oc il)
    end;
    Compilenv.save_unit_info (prefixname ^ ".cmx")

open Parsetree	  
let compile_interface prefixname env ast =
  let modname = String.capitalize (Filename.basename prefixname) in
    Compilenv.reset modname; (* Compilenv is used to retrieve current module name *)
    Naming.init_name_table modname;
  let rec extract_typedefs accu = function
      [] -> List.rev accu
    | {psig_desc = Psig_type tdef}::rem -> 
	extract_typedefs ({pstr_desc=Pstr_type tdef;pstr_loc=Location.none}::accu) rem
    | {psig_desc = Psig_exception (ename,edef)}::rem -> 
	extract_typedefs ({pstr_desc=Pstr_exception (ename,edef);pstr_loc=Location.none}::accu) rem
    | {psig_desc = Psig_module (mname,mtype)}::rem -> 
	begin try
	  let modstruct = extract_module_typedefs mtype in
	    extract_typedefs ({pstr_desc=Pstr_module (mname,modstruct);pstr_loc=Location.none}::accu) rem
	with Failure _ -> List.rev accu
	end
    | {psig_desc = Psig_open omod}::rem -> 
	extract_typedefs ({pstr_desc=Pstr_open omod;pstr_loc=Location.none}::accu) rem
    | {psig_desc = Psig_include imod}::rem -> 
	begin try
	  let modstruct = extract_module_typedefs imod in
	    extract_typedefs ({pstr_desc=Pstr_include modstruct;pstr_loc=Location.none}::accu) rem
	with Failure _ -> List.rev accu
	end
    (* TODO: adress classes here *)
    (*    | {psig_desc = Psig_class cdef}::rem -> 
	  extract_typedefs ({pstr_desc=Pstr_class cdef;pstr_loc=Location.none}::accu) rem
    | {psig_desc = Psig_class_type ctdef}::rem -> 
	extract_typedefs ({pstr_desc=Pstr_class_type ctdef;pstr_loc=Location.none}::accu) rem *)

	(* ignore module types and values *)
    | {psig_desc = Psig_modtype (mtname,Pmodtype_manifest modtype)}::rem ->
	 extract_typedefs ({pstr_desc=Pstr_modtype (mtname,modtype);pstr_loc=Location.none}::accu) rem	 
	 (* what about abstract modtypes ? *)
    | _::rem -> extract_typedefs accu rem
  and extract_module_typedefs mtype = 
    let mexp_desc = match mtype.pmty_desc with
	(* CamIL limitation: interface files with no implementation counterparts *)
	(* that defines types by means of a "module M: MSIG" are not compiled correctly *)
	(* ie. CLI types implementing those types will not be generated !! *)
	(* When CamIL encounters this problem, it does not compile .mli signature any further *)
	(* because forthcoming types may use the unimplemented module signature *)
	(* However, in case there is a corresponding implementation file, everything will be OK *)
	Pmty_ident id -> raise (Failure "not implemented")
      | Pmty_signature sg -> Pmod_structure (extract_typedefs [] sg)
      | Pmty_functor (fname,farg,fmod) -> Pmod_functor (fname,farg,extract_module_typedefs fmod)
      | Pmty_with (modtype,cstrs) -> Pmod_constraint ((extract_module_typedefs modtype),modtype) (* !! verifier !! que faire des contraintes ? *)
    in {pmod_desc=mexp_desc; pmod_loc=Location.none}
  in 
  let fake_structure = extract_typedefs [] ast in
  let tstr,sg2,newenv = Typemod.type_structure env fake_structure in
  let icu  = {icuass=None;icuassref=[];icutypes=[]} in
    Compil.tintfdec prefixname icu;
     if !Clflags.dump_il then 
    (Emitil.init_co stdout ; Emitil.ilcompunit icu);
    begin
      match !Clflags.compilation_mode with
	| Clflags.PlainIL ->     
	    begin
	      let fileil = open_out (prefixname ^ ".il") in 
		Emitil.init_co fileil ;
		Emitil.ilcompunit icu ;
		close_out fileil 
	    end
	| Clflags.MarshalledIL -> Compilenv.set_appending_tocmx_fun (fun oc -> output_value oc icu)
    end;
    Compilenv.save_unit_info (prefixname ^ ".cmx")


(* inspired by asmlink *)
open Asmlink

let exec_ilasm c =
    let c2 = 
      if !Clflags.verbose then (print_string c; print_newline();c) 
      else " /quiet "^(try (ignore(String.index c '>');c) with Not_found -> c ^ " > .trace") in
    if (Sys.command ("ilasm /nologo "^c2))<>0 then raise (Error (Linking_error))

let exec c = 
  let c2 = if !Clflags.verbose 
  then (print_string c; print_newline();c) 
  else 
    try (ignore(String.index c '>');c)
    with Not_found -> c ^ " > .trace" in
    if (Sys.command c2)<>0 then Utils.bug "Exec.Failure" c



let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, _, crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg


let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !Config.load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = Compilenv.read_unit_info file_name in
      if info.Compilenv.ui_compilationmode <> !Clflags.compilation_mode 
      then raise (Error (Inconsistent_compilation_modes(obj_name,info.Compilenv.ui_compilationmode,!Clflags.compilation_mode)));
      remove_required info.Compilenv.ui_name;
      List.iter (add_required file_name) info.Compilenv.ui_imports_cmx;
      (info, file_name, crc) :: tolink
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    (* This is an archive file. *)
    (* TODO Each unit contained in it will be linked
       in only if needed. *)
    let ic = open_in_bin file_name in
      (* TODO check magic number
	 let buffer = String.create (String.length cmxa_magic_number) in
	 really_input ic buffer 0 (String.length cmxa_magic_number);
	 if buffer <> cmxa_magic_number then
	 raise(Error(Not_an_object_file file_name)); *)
    try
      let refl = (input_value ic : (string*string) list) in
	close_in ic;
	List.iter (fun (modname,assref)-> remove_required modname) refl;
	tolink
    with _ ->  raise(Error(Not_an_object_file ("(DEBUG CAMIL):"^file_name)))
	
  end
  else raise(Error(Not_an_object_file file_name))


let output_config_file exec_exename =
  let config_file=open_out (exec_exename^".config") in
    output_string config_file "<configuration>\n  <runtime>\n    <assemblyBinding xmlns=\"urn:schemas-microsoft-com:asm.v1\">\n";
    let tokenstring name = 
      let tok,_ = get_token_version name in
	if tok<>"" then (" publicKeyToken = \""^tok^"\"") else "" in	    
    let config_item name = 
      let codebase = (
	if name = Naming.core_camil_assbly then (
	  if bootstrap then camil_core_fullpath
	  else path_to_url camil_core_fullpath
	)
	else (try (find_codebase name)^".dll"
	      with Not_found -> (path_to_url standard_library)^"/"^name^".dll"
	     )
      ) in
	output_string config_file "      <dependentAssembly>\n";
	output_string config_file ("        <assemblyIdentity name=\""^name^"\"");
	output_string config_file ((tokenstring name)^"/>\n");
	output_string config_file ("        <codeBase version =\"0.0.0.0\" href=\""^codebase^"\"/>\n");
	output_string config_file "      </dependentAssembly>\n";
    in
      List.iter (fun assref -> config_item assref) (external_assemblies()); 
      output_string config_file "    </assemblyBinding>\n  </runtime>\n</configuration>\n";
      close_out config_file


let create_cmxa exec_exename exec_bname = 
  let cmxa = 	
    try 
      ((Filename.chop_extension exec_exename)^".cmxa")
    with Invalid_argument _ -> exec_exename^".cmxa" in
  let refs = retrieve_nocore_refs() in
  let oc = open_out_bin cmxa in
    output_value oc (List.map (fun (modname,assref) -> if assref="" then (modname,exec_bname) else (modname,assref)) refs);
    close_out oc
      

let detect_missing_implementations() =
  Array.iter remove_required Runtimedef.builtin_exceptions;
  List.iter remove_required (List.map String.capitalize stdlib_items); (* TODO PB : do not remove a stdlib module if redefined by user code *)
    begin match  extract_missing_globals() with
	[] -> ()
      | mg ->  raise(Error(Missing_implementations mg))
    end

let write_external_assemblies_references icu = 
  Hashtbl.iter (fun assnma aref ->  icu.icuassref <- aref::icu.icuassref) assemblyref_table

let write_assembly_header icu exec_bname exec_exename_nodir =
  let assbly = {
    aver="0:0:0:0";
    amod=exec_exename_nodir;
    aflg=false;
    akey="";
    anme=Filename.basename exec_bname
  } in 
    icu.icuass <- Some assbly

let write_MLTop_fields mltop_class = 
  (* startup flag *)
  let flag_fld = Ilbuild.new_field mltop_class ("startup_flag") Ilpredef.bool_type in
  Ilbuild.add_fatt flag_fld FAprivate;
  Ilbuild.add_fatt flag_fld FAstatic;

  (* optimised constant constructors *)
  if !Clflags.compiling_camil_corelib then begin
    let const_ctr_type = 
      if not !Clflags.rebuiltmode && not !Clflags.variantrepr_objarray then Ilpredef.variant_type
      else Ilpredef.objtab_type in
      for i = 0 to 255 do
	let fld = Ilbuild.new_field mltop_class ("const_ctr"^(string_of_int i)) const_ctr_type in
	  Ilbuild.add_fatt fld FApublic;
	  Ilbuild.add_fatt fld FAstatic
      done
  end
      
let write_MLTop_startupAll_setup_fields startupModules =
  if !Clflags.compiling_camil_corelib then begin
  (* optimised constant constructors *)
    if !Clflags.rebuiltmode || !Clflags.variantrepr_objarray then 
      begin
	for i = 0 to 255 do
	  Inst.ldci4 startupModules 1;
	  Inst.newarr_spec startupModules Tobject;
	  Inst.dup startupModules;
	  Inst.ldci4 startupModules 0;
	  Inst.ldci4 startupModules i;
	  Inst.box_spec startupModules Tint32;
	  Inst.stelem_ref startupModules;
	  let fr = Ilbuild.field_ref thisMLTop ("const_ctr"^(string_of_int i)) Ilpredef.objtab_type in
	    Inst.stsfld startupModules fr
	done end
    else begin
      for i = 0 to 255 do
	Inst.ldci4 startupModules i;
	Inst.newobj startupModules Ilpredef.variant_optim_cc_ref ".ctor" Il.Tvoid [Tint32,None];
	let fr = Ilbuild.field_ref thisMLTop ("const_ctr"^(string_of_int i)) Ilpredef.variant_type in
	  Inst.stsfld startupModules fr
      done 
    end
  end

let write_MLTop_startupAll mltop_class startup_list =
  let startupAll = Ilbuild.new_method mltop_class "startupAll" Tvoid [] in
    Ilbuild.add_matt startupAll MApublic;
    Ilbuild.add_matt startupAll MAstatic;
    Ilbuild.add_miat startupAll IAmanaged;
    (* some initialisations *)
    write_MLTop_startupAll_setup_fields startupAll;
  (* starting up stdlib *)
    if !Clflags.compiling_camil_corelib then 
      begin
	Inst.ldnull startupAll;
	let syscamilref = Ilbuild.type_ref "CamIL" "Sys" in
	  Inst.call_method startupAll (Ilbuild.method_ref NoInstance syscamilref "set_argv" Tvoid [Tobject,None]) 
      end;
  (* starting up all dependencies linked together *)
  let start objfile =
    let topref = 
      let basename = Filename.chop_extension (Filename.basename objfile) in
      if Filename.check_suffix objfile "cmx" then
	let modname = String.capitalize basename in
	  Ilbuild.type_ref modname "Top" 
      else
	if Filename.check_suffix objfile "cmxa" then
	  let topref = Ilbuild.type_ref "" "MLTop" in (* PB : [assembly_ext]MLTop *)
	    topref.trscp <- build_extscope basename;
	    topref
	else assert false
    in
      Inst.call_method startupAll (Ilbuild.method_ref NoInstance topref "startup" Tvoid [])
  in List.iter start startup_list;
    Inst.ret startupAll

let write_MLTop_reset mltop_class startup_list =
  let reset = Ilbuild.new_method mltop_class "reset" Tvoid [] in
    Ilbuild.add_matt reset MApublic;
    Ilbuild.add_matt reset MAstatic;
    Ilbuild.add_miat reset IAmanaged;
    if not !Clflags.compiling_camil_corelib then
      (let camiltopref = Ilbuild.type_ref "" "MLTop" in 
	 camiltopref.trscp <- build_extscope Naming.core_camil_assbly;
	 Inst.call_method reset (Ilbuild.method_ref NoInstance camiltopref "reset" Tvoid []));
    let reset_ext objfile =
      let basename = Filename.chop_extension (Filename.basename objfile) in
	if Filename.check_suffix objfile "cmxa" then
	  let topref = Ilbuild.type_ref "" "MLTop" in (* PB : [assembly_ext]MLTop *)
	    topref.trscp <- build_extscope basename;
	    Inst.call_method reset (Ilbuild.method_ref NoInstance topref "reset" Tvoid []) 
	else ()
    in List.iter reset_ext startup_list;
      let flag_fld = Ilbuild.field_ref thisMLTop "startup_flag" Ilpredef.bool_type in
	Inst.ldci4 reset 0;
	Inst.stsfld reset flag_fld;
	Inst.ret reset
	  

let write_MLTop_startup mltop_class =
  let startup = Ilbuild.new_method mltop_class "startup" Tvoid [] in
    Ilbuild.add_matt startup MApublic;
    Ilbuild.add_matt startup MAstatic;
    Ilbuild.add_miat startup IAmanaged;
    if not !Clflags.compiling_camil_corelib then 
      (let camiltopref = Ilbuild.type_ref "" "MLTop" in 
	 camiltopref.trscp <- build_extscope Naming.core_camil_assbly;
	 Inst.call_method startup (Ilbuild.method_ref NoInstance camiltopref "startup" Tvoid []));
    let flag_fld = Ilbuild.field_ref thisMLTop "startup_flag" Ilpredef.bool_type in
    let lbl_end = Ilbuild.new_label startup in
      Inst.ldsfld startup flag_fld;
      ignore(Inst.brtrue startup lbl_end); 
      Inst.call_method startup (Ilbuild.method_ref NoInstance thisMLTop "startupAll" Tvoid []);
      Inst.ldci4 startup 1;
      Inst.stsfld startup flag_fld;
      Inst.label_follow startup lbl_end; 
      Inst.ret startup
	

let write_MLTop_main mltop_class =
  let main = Ilbuild.new_method mltop_class "main" Tint32 [Tvector Tstring,None] in
    Ilbuild.add_matt main MApublic;
    Ilbuild.add_matt main MAstatic;
    Ilbuild.add_miat main IAmanaged;
    main.entrypoint <- true;
    let lbl1 = Ilbuild.new_label main in
    let lbl2 = Ilbuild.new_label main in
      Inst.try_main main;
	Inst.call_method main (Ilbuild.method_ref NoInstance thisMLTop "startup" Tvoid []);
	ignore(Inst.leave main lbl1);
	Inst.try_catch main;
	Inst.new_local main "tmp" (Tvector Tobject);
	Inst.force_stack main (if !Clflags.noILexceptionHandling then Ilpredef.exn_type 
 				   else Ilpredef.sysexn_type);
	if not !Clflags.noILexceptionHandling then begin
	  let lbl_embedexc = Ilbuild.new_label main 
	  and lbl_camilexc = Ilbuild.new_label main in
	    Inst.dup main ;
            Inst.isinst main Ilpredef.exn_ref ;
	    ignore (Inst.brfalse main lbl_embedexc);
	    Inst.castclass main Ilpredef.exn_ref;
	    ignore (Inst.br main lbl_camilexc);
	    Inst.label_follow main lbl_embedexc ;
	    Inst.call_method main Ilpredef.exn_embed ;
	    Inst.label_follow main lbl_camilexc 
	end;
	Inst.ldfld_field main Ilpredef.exn_field Instance;
	Inst.stloc_name main "tmp";
	Inst.ldloc_name main "tmp";
	let consoleref = Ilbuild.type_ref Ilpredef.system_ns "Console" in
	let textwriterref = Ilbuild.type_ref (Ilpredef.system_ns^".IO") "TextWriter" in
	  Inst.call_method main (Ilbuild.method_ref NoInstance consoleref "get_Error" (Tclass textwriterref) []);
	  let setout= Ilbuild.method_ref NoInstance consoleref "SetOut" Tvoid [Tclass textwriterref,None] in
	    Inst.call_method main setout;
	    Inst.ldstr main "Fatal Error: exception ";
	    let write = Ilbuild.method_ref NoInstance consoleref "Write" Tvoid [Tstring,None] in
	    let writeline = Ilbuild.method_ref NoInstance consoleref "WriteLine" Tvoid [Tstring,None] in
	      Inst.call_method main write;
	      Inst.ldloc_name main "tmp";

(* Printexc.to_string has runtime type object[] -> object with type reconstruction *)
(* and object[] -> char[] with type propagation *)
	      if !Clflags.rebuiltmode then begin 
		Inst.call_method main (Ilbuild.method_ref NoInstance (Ilbuild.type_ref ("Printexc.closures") "_to_string")
					 "exec" Tobject [Tvector Tobject,None]);
		Inst.call_method main Ilpredef.chararray_of_object
	      end else Inst.call_method main (Ilbuild.method_ref NoInstance (Ilbuild.type_ref ("Printexc.closures") "_to_string")
						"exec" Ilpredef.char_arr_type [Tvector Tobject,None]);

	      Inst.newobj_ctor main Ilpredef.string_ctor;
	      Inst.call_method main writeline;
	      Inst.call_method main (Ilbuild.method_ref NoInstance consoleref "get_Out" (Tclass textwriterref) []);
	      Inst.call_method main setout;

  (* TODO: thread-safe code: lock the modification of stdout *)
	      ignore(Inst.leave main lbl2);
	      Inst.try_end main ;
	      Inst.label_follow main lbl1 ;
	      Inst.ldci4 main 0;
	      Inst.ret main ;
	      Inst.label_follow main lbl2 ;
	      Inst.ldci4 main 1;
	      Inst.ret main

let write_class_MLTop icu startup_list =
  let mltop_class = Ilbuild.new_class icu "" "MLTop" in
    write_MLTop_fields mltop_class;
    write_MLTop_startupAll mltop_class startup_list;
    write_MLTop_reset mltop_class startup_list;
    write_MLTop_startup mltop_class;
    if not !Clflags.make_dll then write_MLTop_main mltop_class

let write_self_assembly icu exec_bname exec_exename_nodir startup_list =
  write_assembly_header icu exec_bname exec_exename_nodir;
  write_class_MLTop icu startup_list

let link_resolve_references_plainIL exec_il exec_exename exec_ilname objfiles =
  close_out exec_il ;
  let substitutions = List.fold_left (
    fun s (modname,assref) -> s^" -e \"s|\\[?"^modname^"\\]|["^assref^"]|g\"") "sed" (retrieve_refs()) in
  let cmx2il cmx = (try (Filename.chop_extension cmx) with Invalid_argument _ -> cmx) ^".il" in
    List.iter (fun cmx -> let ilname = cmx2il cmx in exec (substitutions^" "^ilname^" > "^ilname^"bis")) 
      (List.filter (fun file -> Filename.check_suffix file ".cmx") objfiles);
    let flags = "/out="^ exec_exename^(if !Clflags.make_dll then " /dll " else " ")
	      ^exec_ilname^" "
	      ^(if !Clflags.inlined_il = [] then "" else (List.fold_right  (fun x y -> x^" "^y) !Clflags.inlined_il ""))
	      ^(List.fold_right  (fun x y -> (cmx2il x)^"bis "^y) objfiles "") 
	      ^(if (String.compare !Clflags.snk_file "") <> 0 then " /key:"^(!Clflags.snk_file) else "")
	      ^(if !Clflags.lightning_debug then " /debug" else "")
    in 
      exec_ilasm flags

let link_resolve_references icu exec_il exec_exename exec_ilname objfiles = 	
  let appendIL objfile = 
    let ic = open_in_bin objfile in
      Compilenv.skip_header_unit_info ic objfile; (* *)
      let ilcode = (input_value ic:il_compilation_unit) in
	close_in ic;
	merge_icus ilcode icu
  in
    List.iter appendIL (List.filter (fun file -> Filename.check_suffix file ".cmx" ) objfiles);
    resolve_all_references icu;  
    if !Clflags.reflection_linker then
      let _ = Dynemit.ilcompunit icu exec_exename !Clflags.snk_file in ()
    else begin
      Emitil.init_co exec_il; (* emits in append mode *)
      Emitil.ilcompunit icu;
      close_out exec_il;
      let flags = "/out="^ exec_exename^(if !Clflags.make_dll then " /dll " else " ")
		  ^ exec_ilname ^ " "
		  ^ (if !Clflags.inlined_il = [] then "" else (List.fold_right  (fun x y -> x^" "^y) !Clflags.inlined_il ""))
		  ^ (if !Clflags.snk_file <> "" then " /key:"^(!Clflags.snk_file) else "")
		  ^ (if !Clflags.lightning_debug then " /debug" else "")	
      in 
	exec_ilasm flags
    end	

(* TODO: use reflection linker instead ? ... *)
let link_resolve_references_toplevel icu = 	
  resolve_all_references icu;  
  let assbuilder = Dynemit.ilcompunit icu "" !Clflags.snk_file in
    Obj.magic (assbuilder#_get_cts_cAssemblyBuilder)

let link objfiles = 
  let icu  = {icuass=None;icuassref=[];icutypes=[]} in
    if !Clflags.verbose then (  List.iter (fun x->prerr_string (x^"\n")) objfiles; flush stderr);
    initialize_assemblyref();
    Config.load_path := "" :: List.rev_append !Clflags.include_dirs (Clflags.std_include_dir ());

  (* Detection of dependencies problems *)
  let units_tolink = List.fold_right scan_file objfiles [] in (* !! gardé pour son effet de bord ? *)
    detect_missing_implementations();
      (* we now fill-in the table mapping references to actual assemblies, following the ordering of object files *)
      (* 1. for module.cmx we build a map between the empty reference and the assembly currently being built (previous references may be overwritten) *)
      (* 2. we open archive.cmxa to add its own mapping information *)
      let add_reference objfile =
	try
	if (Filename.check_suffix objfile "cmx") then
	  let modname = (String.capitalize (Filename.chop_extension (Filename.basename objfile)))
	  in new_assemblyref modname ""
	else (* could be a .cmxa *)
	  begin
	    if Filename.check_suffix objfile "cmxa" then
	      let file_name =
		try
		  find_in_path !Config.load_path objfile
		with Not_found ->
		  raise(Error(File_not_found objfile)) in
	      let ic = open_in_bin file_name in
	      let refl = (input_value ic : (string*string) list) in
		close_in ic;
		(* here we add the cmxa location to the path, to make sure the associated dll is in the research path *)
		(* of the tokenstring function that retrieves the dll public key *)
		Config.load_path := !Config.load_path@[Filename.dirname file_name];
		(* *)
		List.iter (fun (modname,assref)-> new_assemblyref modname assref) refl
	    else Utils.bug "Ilcompile" "link"
	  end
	with Not_found -> failwith ("Ilcompile.link: pb with "^objfile)
      in 
	List.iter add_reference objfiles;

	let exec_exename = access_name !Clflags.output_name in 
	let exec_exename_nodir = Filename.basename exec_exename in
	let exec_dir = Filename.dirname exec_exename in
	let exec_bname = 
	  try (Filename.chop_extension exec_exename_nodir)
	  with Invalid_argument _ -> exec_exename_nodir in
	let exec_ilname = exec_dir^"/_"^exec_bname ^ ".il" in 
	  
	  (* write cmxa when building a dll *)
	  if !Clflags.make_dll then create_cmxa exec_exename exec_bname;
	  
	  (* emit ".assembly extern" declarations *)
	  let exec_il = open_out exec_ilname in 
	    write_external_assemblies_references icu;
	    (* emit the current assembly itself *)
	    write_self_assembly icu exec_bname exec_exename_nodir objfiles;
	    
	    (* actual link *)
	    begin
	      match !Clflags.compilation_mode with
		| Clflags.PlainIL -> link_resolve_references_plainIL exec_il exec_exename exec_ilname objfiles
		| Clflags.MarshalledIL -> link_resolve_references icu exec_il exec_exename exec_ilname objfiles
	    end;
	    (* emit XML config file *)
	    output_config_file exec_exename;
	    
	    if !Clflags.peverify then exec ("peverify.exe " ^ exec_exename)
