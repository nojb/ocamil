(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* $Id: ilpath.ml,v 1.22 2006/10/17 11:36:38 montela Exp $ *)
open Config;;
open Il;;

let slash_char = (Filename.concat "a" "a").[1];;
let slash_string = String.make 1 slash_char;;

let rec valenv l default = 
  match l with 
     [] -> default
   | h::q -> 
       (try 
	 let env = Sys.getenv h in
	 let l=String.length env in
	 if l>1 && env.[0] = '"' && env.[l-1] = '"' then String.sub env 1 (l-2) else env
       with _ -> valenv q default);;


(* AJOUTS RAPH *)

(* cherche l'emplacement d'un assembly archive CamIL (couple dll/cmxa) *)
(* par le biais du cmxa, la dll devant etre au meme endroit *)
(* la recherche se fait dans le loadpath *)
let find_codebase name =
  let essais = ref ("."::!Config.load_path) in
    while (!essais <> [] && (not (Sys.file_exists ((List.hd !essais)^slash_string^name^".dll")))) do 
      (  if !Clflags.verbose then (Printf.printf "+%s%s%s.dll\n" (List.hd !essais) slash_string name;flush stdout); 
	essais:= List.tl !essais) done;
    if !essais <> [] then (
      if !Clflags.verbose then (Printf.printf "Codebase de %s trouve en %s\n" name (List.hd !essais));
      (List.hd !essais)^slash_string^name
    ) 
    else ( 
      if !Clflags.verbose then (print_endline ("Codebase absent du loadpath : "^name));
      raise Not_found)

let stdlib_items = ["pervasives" ; "CLIinteraction"; "array" ; "list" ;  "char" ; "string" ; 
		    "sys" ; "complex"; "buffer" ;  "map" ;  "obj";  "queue" ; 
		    "sort" ; "hashtbl" ;"int32";"int64";"nativeint" ; "printf" ; "digest" ; "random" ; "camlinternalOO" ; "set" ; 
		    "filename" ; "format"; "arg"; "stack" ; "printexc" ;  
		    "weak" ; "gc" ; "lexing" ; "parsing" ; "lazy";"marshal";"oo";"genlex";"stream";"scanf";
		    "arrayLabels" ; "listLabels" ; "stringLabels" ; "stdLabels" ; "moreLabels" ; "jacare" ; "ctsHierarchy" ; "ctsArray" ];;


(* get public key token of assembly [name] *)

let assemblyinfo_cache = ref ([]:(string * ((string * string) * Painfo.assembly option)) list)

let get_assemblyinfo name = (* may raise Not_found when Painfo fails *)
  try List.assoc name !assemblyinfo_cache
  with Not_found -> begin
    let ainfo =
      if name="mscorlib" then Painfo.read_assemblyinfo "mscorlib.dll"
      else if name=Naming.core_camil_assbly && Config.bootstrap then 
	let (tok,_),assbly = Painfo.read_assemblyinfo_of_core_camil() in ((tok,"0:0:0:0"),assbly) (* ?? *)
      else if List.mem name stdlib_items then failwith ("Ilpath: Abnormal token demand "^name) 
      else if name <> Naming.core_camil_assbly then Painfo.read_assemblyinfo ((find_codebase name)^".dll")
      else 
	try
	  Painfo.read_assemblyinfo camil_core_fullpath
	with Not_found -> (* when compiling graphics, etc.. core_camil is not installed yet but in . *)
	  Painfo.read_assemblyinfo ((find_codebase name)^".dll") 
    in
      assemblyinfo_cache := (name,ainfo)::(!assemblyinfo_cache);
      ainfo
  end


let get_token_version name =
  try 
    let (tok,ver),_ = get_assemblyinfo name in (tok,ver)
  with Not_found -> failwith ("Cannot get token,version for " ^ name)

let get_assembly_object name =
  try
    let _,a = get_assemblyinfo name in Utils.someof a
  with Not_found -> failwith ("Cannot get assembly object for " ^ name)

(* get public key token of ocamil compiler itself *)
let get_token_self () =
  let (token,_),_ = Painfo.read_assemblyinfo_of_self() in token
	  
let path_to_url s = "file://"^s

let build_extref assbly =
  let tok,ver = get_token_version assbly in 
    {
      arver=ver;
      arnme=assbly;
      arflg=false;
      arkey=tok;
      arhsh=""
    }

let build_extscope assbly =
  ExternalAssembly (build_extref assbly)
