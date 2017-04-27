(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

(* pretty-print of il values to generate an IL assembly file *)

open Utils
open Il
open Ilpath

(* functions for easy printing *)

let co = ref stdout 
let init_co ch = co:=ch 

let pc c = output_char !co c 
let ps s = output_string !co s 
let pnl () = output_char !co '\n'
let pel s = ps s ; pnl () 
let pf f x = Printf.fprintf !co f x

let ifnn f l = if l<>[] then  f l
let ifno f o = match o with None -> () | Some x -> f x 
let ms f = fun x -> f x ; ps " " 

(* pretty-print of attributes *)

let classattr = function 
    CAabstract        -> ps "abstract"
  | CAansi            -> ps "ansi"
  | CAauto            -> ps "auto"
  | CAautochar        -> ps "autochar"
  | CAexplicit        -> ps "explicit"
  | CAinterface       -> ps "interface"
  | CApublic          -> ps "public"
  | CAsealed          -> ps "sealed"
  | CAsequential      -> ps "sequential"
  | CAunicode         -> ps "unicode"
  | CArtspecialname   -> ps "rtspecialname"
  | CAspecialname     -> ps "specialname"
  | CAserializable    -> ps "serializable"
  | CAbeforefieldinit -> ps "beforefieldinit"
  | CAprivate         -> ps "private"
  | CAnestedpublic    -> ps "nested public"
  | CAnestedprivate   -> ps "nested private"
  | CAnestedfamorassem-> ps "nested famorassem"
  | CAnestedfamily    -> ps "nested family"
  | CAnestedfamandassem-> ps "nested famandassem"
  | CAnestedassembly  -> ps "nested assembly"

let rec fieldattribute = function 
    FAassembly      -> ps "assembly" 
  | FAfamandassem   -> ps "amandassem"
  | FAfamily        -> ps "family"
  | FAfamorassem    -> ps "famorassem"
  | FAinitonly      -> ps "initonly"
  | FAliteral       -> ps "literal"
  | FAnotserialized -> ps "notserialized"
  | FApinvokeimpl   -> ps "pinvokeimpl"
  | FAprivate       -> ps "public" (* MODIF: handle "private" properly *)
  | FApublic        -> ps "public"
  | FAstatic        -> ps "static"
  | FArtspecialname -> ps "rtspecialname"
  | FAspecialname   -> ps "specialname"
  | FAhasfieldRVA   -> ps ""
  | FAhasdefault    -> ps ""
  | FAhasfieldmarshal -> ps ""

let methodattribute = function 
    MAabstract      -> ps "abstract"
  | MAassembly      -> ps "assembly"
  | MAfamandassem   -> ps "famandassem"
  | MAfamily        -> ps "family"
  | MAfamorassem    -> ps "famorassem"
  | MAfinal         -> ps "final"
  | MAhidebysig     -> ps "hidebysig"
  | MAprivate       -> ps "public" (* MODIF: handle "private" properly *)
  | MApublic        -> ps "public"
  | MArtspecialname -> ps "rtspecialname"
  | MAspecialname   -> ps "specialname"
  | MAstatic        -> ps "static"
  | MAvirtual       -> ps "virtual"
  | MArequiresecobject -> ps ""
  | MAhassecurity -> ps ""
  | MApinvokeimpl -> ps ""

let callconv = function 
    Instance -> ps "instance "
  | NoInstance -> ()

and implementattribute = function
    IAil           -> ps "il"
  | IAmanaged      -> ps "managed"
  | IAnative       -> ps "native"
  | IAoptil        -> ps "optil"
  | IAruntime      -> ps "runtime"
  | IAunmanaged    -> ps "unmanaged"
  | IAforwardref   -> ps "forwardref"
  | IApreservesig  -> ps "preservesig"
  | IAsynchronized -> ps "synchronized"
  | IAnoinlining   -> ps "noinlining"

(* ident, qualified name, etc... *)

let replace_carret s =
    for i=0 to (String.length s)-1 do
      if s.[i]='^' then s.[i]<-'$'
    done

let str_class id = 
  replace_carret id.trnme; (* for instance because of Pervasives.(^) *)
  let qid = if id.trnsp="" then Naming.quote_qualified id.trnme else Naming.quote_qualified (id.trnsp^"."^id.trnme) in
  let assref = match id.trscp with 
    | UnknownAssembly -> "[????]"
    | ThisAssembly -> ""
    | ExternalAssembly assref -> "["^assref.arnme^"]"
  in assref ^ qid
    

let str_field id = (str_class id.frcla) ^ "::" ^ (Naming.quote id.frnme)
let str_method id = (str_class id.mrcla) ^ "::" ^ (Naming.quote id.mrnme)
let str_space id = Naming.quote_qualified id

let ident id = ps (Naming.quote id)
let namespace id = ps (str_space id) 
let classname id = ps (str_class id)
let methodname id = ps (str_method id)
let fieldname id = ps (str_field id)

(* types *)

let rec iltype = function 
    Tclass n -> ps "class " ; classname n 
  | Tobject  -> ps "object"
  | Tstring  -> ps "string"
  | Tvaluetype n -> ps "value class "; classname n 
  | Tvector t -> iltype t ; ps "[]"
  | Tampersand t -> iltype t ; ps "&" 
  | Tstar t  -> iltype t ; ps "*" 
  | Tchar    -> ps "char"
  | Tvoid    -> ps "void"
  | Tbool    -> ps "bool"
  | Tint8    -> ps "int8"
  | Tint16   -> ps "int16"
  | Tint32   -> ps "int32"
  | Tint64   -> ps "int64"
  | Tfloat32 -> ps "float32"
  | Tfloat64 -> ps "float64"
  | Tuint8   -> ps "unsigned int8"
  | Tuint16  -> ps "unsigned int16"
  | Tuint32  -> ps "unsigned int32"
  | Tuint64  -> ps "unsigned int64"
  | Tnint    -> ps "native int"
  | Tnuint   -> ps "native unsigned int" 

let iltyperef_spec = function
    TypeRef tr -> classname tr
  | TypeSpec t -> iltype t


(* variables and signatures *)

let variable = function
    (t,None) -> iltype t 
  | (t,Some id) -> iltype t ; ps " " ; ident id

let signature = 
  let rec aux = function 
      [] -> ()
    | [v] -> variable v 
    | v::l -> variable v ; ps "," ; aux l  in 
  function vl -> ps "(" ; aux vl ;  ps ") " 
  

(* to include tabular in generated IL code *)

let tabtxt = "  " 
let tabref = ref 0 
let tab () = incr tabref 
let untab () = decr tabref 
let tp () = for i=1 to !tabref do ps tabtxt done 

let ptab f x = tp () ; f x 
let ptabel f x = tp () ; f x ; pnl ()


(* instructions *)

let opt_ldarg n = 
  if n>=0 && n<4 then pf "ldarg.%d" n 
  else if n<256 then pf "ldarg.s %d" n 
  else pf "ldarg %d" n 

let opt_starg n = 
  if n<256 then pf "starg.s %d" n 
  else pf "starg %d" n 

let opt_ldci4 n = 
  if n>=0 && n<9 then pf "ldc.i4.%d" n 
  else if n=(-1) then ps "ldc.i4.M1" 
  else if n>=(-128) && n<=127 then pf "ldc.i4.s %d" n
  else pf "ldc.i4 %d" n 

let opt_ldloc n = 
  if n<4 then  pf "ldloc.%d" n 
  else if n<256 then pf "ldloc.s %d" n 
  else pf "ldloc %d" n 

let opt_stloc n = 
  if n<4 then  pf "stloc.%d" n 
  else if n<256 then pf "stloc.s %d" n 
  else pf "stloc %d" n 

let is_printable x = let y = Char.code x in y>31 && y<128 

let print_escaped_string str =
  for i=0 to (String.length str)-1 do 
    match str.[i] with 
	'\n' -> ps "\\n" 
	| '\b' -> ps "\\b"
	| '\\' -> ps "\\\\"
	| '\"' -> ps "\\\""
	| x -> pc x 
  done 
  
let print_literal_string str =
  try (
    for i=0 to (String.length str)-1 do 
      if not (is_printable str.[i]) then raise (Invalid_argument "not_printable")
    done;
    ps "\"";print_escaped_string str;ps "\"")
  with Invalid_argument "not_printable" ->
    ps "bytearray (";
    for i=0 to (String.length str)-1 do
      ps (Printf.sprintf "%2.2X 00 " (Char.code str.[i]))
    done;
    ps ")"
	
let instruction = function 
    Icomment s       -> pf "// %s" s 
  | Ilabel s         -> pf "%s:" s 
  | Itry_main        -> ps ".try" ; pnl () ; tp () ; ps "{" ; tab () 
  | Itry_catch       -> untab () ; ps "}" ; pnl () ; tp () ; 
      if !Clflags.noILexceptionHandling then 
	ps ("catch "^(if  !Clflags.nopervasives || !Clflags.compiling_camil_corelib then "" 
		      else "["^Naming.core_camil_assbly^"]")^"CamIL.Exception ")
      else  ps ("catch [mscorlib]System.Exception "); 
      pnl(); tp () ; ps "{" ; tab () 
  | Itry_end         -> untab () ; ps "}" 

  | Iadd             -> ps "add" 
  | Iand             -> ps "and" 
  | Ibeq s           -> pf "beq %s" s 
  | Ibge s           -> pf "bge %s" s 
  | Ibgt s           -> pf "bgt %s" s 
  | Ible s           -> pf "ble %s" s 
  | Iblt s           -> pf "blt %s" s 
  | Ibox t           -> ps "box " ; iltyperef_spec t
  | Ibr s            -> pf "br %s" s 
  | Ibrtrue s        -> pf "brtrue %s" s 
  | Ibrzero s        -> pf "brfalse %s" s
  | Icall mref -> 
      ps "call "; callconv mref.mrccn ; ps " "; iltype mref.mrtyp; ps " ";
      ps " " ; methodname mref ; signature mref.mrprm 
  | Icallvirt mref -> 
      ps "callvirt "; callconv mref.mrccn ; ps " "; iltype mref.mrtyp; ps " ";
      ps " " ; methodname mref ; signature mref.mrprm 
  | Inewobj mref -> ps "newobj ";callconv mref.mrccn ; ps " "; iltype mref.mrtyp; ps " ";
      ps " " ; methodname mref ; signature mref.mrprm 
  | Iconvi4          -> ps "conv.i4";
  | Iconvi8          -> ps "conv.i8";
  | Iconvi           -> ps "conv.i";
  | Icastclass t     -> ps "castclass " ; iltyperef_spec t 
  | Iceq             -> ps "ceq"
  | Icgt             -> ps "cgt"
  | Iclt             -> ps "clt"
  | Icgt_un          -> ps "cgt.un"
  | Iclt_un          -> ps "clt.un"
  | Idiv             -> ps "div"
  | Idup             -> ps "dup"
  | Ildarg n         -> opt_ldarg n 
  | Ildci4 n         -> opt_ldci4 n 
  | Ildci8 n         -> pf "ldc.i8 %d" (Int64.to_int n)
  | Ildcr8 n         -> pf "ldc.r8 %f" n
  | Ildindi1         -> ps "ldind.i1"
  | Ildindi2         -> ps "ldind.i2"
  | Ildindi4         -> ps "ldind.i4"
  | Ildindi8         -> ps "ldind.i8"
  | Ildindi          -> ps "ldind.i"
  | Ildindr8         -> ps "ldind.r8"
  | Ildelem s        -> pf "ldelem.%s" s
  | Ildfld fref      -> ps "ldfld " ; iltype fref.frsig ; ps " " ; fieldname fref
  | Ildlen           -> ps "ldlen"
  | Ildloc n         -> opt_ldloc n
  | Ildnull          -> ps "ldnull" 
  | Ildsfld fref   -> ps "ldsfld " ; iltype fref.frsig ; ps " " ; fieldname fref
  | Ildstr s         -> ps "ldstr " ; print_literal_string s
  | Ileave s         -> pf "leave %s" s 
  | Imul             -> ps "mul"
  | Ineg             -> ps "neg"
  | Inewarr cn       -> ps "newarr " ; iltyperef_spec cn 
  | Inot             -> ps "not"
  | Ior              -> ps "or" 
  | Ipop             -> ps "pop" 
  | Irem             -> ps "rem"
  | Iret             -> ps "ret"
  | Ishl             -> ps "shl"
  | Ishr             -> ps "shr"
  | Ishrun           -> ps "shr.un"
  | Istelem s        -> pf "stelem.%s" s
  | Istfld fref    -> ps "stfld " ;  iltype fref.frsig ; ps " " ; fieldname fref
  | Istloc n         -> opt_stloc n
  | Istarg n         -> opt_starg n
  | Istsfld fref   -> ps "stsfld " ; iltype fref.frsig ; ps " " ; fieldname fref
  | Isub             -> ps "sub"
  | Iswitch lbl      -> ps "switch (" ;
                        iter_except_last ps (fun () -> ps ",") lbl ;
                        ps ")"
  | Itail            -> ps "tail."
  | Ithrow           -> ps "throw"
  | Iunbox t         -> ps "unbox " ; iltyperef_spec t
  | Ixor             -> ps "xor" 
  | Iisinst t         -> ps "isinst " ; iltyperef_spec t
  | Irethrow           -> ps "rethrow"

let tabinst i = tp () ; instruction i ; pnl () 

let code l = List.iter tabinst (List.rev l) 

(* structures *)

let rec ilcompunit icu =
   let emitassref ar = 
    ps ".assembly extern "; ps (Naming.quote ar.arnme) ; pel "{";
    ps ".publickeytoken = (" ; ps ar.arkey; pel ")";
    ps ".ver "; ps ar.arver ; pel "}" 
  in
    List.iter emitassref icu.icuassref;
     begin
       match icu.icuass with 
	   Some a -> 
	     ps ".assembly "; ps (Naming.quote a.anme) ; ps " {" ; pnl();
	     ps ".custom instance void [mscorlib]System.Security.AllowPartiallyTrustedCallersAttribute::.ctor() = ( 01 00 00 00 )" ; pnl(); 
	     ps ".hash algorithm 0x00008004";pnl();
	     ps ".ver ";ps a.aver;pnl();pel "}";
	  ps ".module ";ps a.amod;pnl()
	 | None -> ()
     end;
     List.iter ilclass icu.icutypes
	
and ilclass cl =
  tp () ; 
  if cl.tdnsp<>"" then (ps ".namespace " ; namespace cl.tdnsp ;
			ps " {"; tab (); pnl(); tp());
  ps ".class public " ;
  ifnn (List.iter (ms classattr)) cl.tdatt ;
  ident cl.tdnme ;
  ifno (fun x -> ps " extends " ;classname x ) cl.tdext ;
  ifnn (fun x -> ps " implements "; List.iter (ms classname) x) cl.tdimp ;  
  pnl () ;
  tp () ; pel "{" ; tab () ;
  List.iter ilfield cl.tdfld ;
  List.iter ilmethod cl.tdmet ;
  untab () ; tp () ;  ps "} // end of class " ; classname (Ilbuild.typeref_of_typedef cl) ; pnl(); 
  if cl.tdnsp<>"" then (  untab(); tp () ; ps "}" ; pnl () )

and ilfield fd = 
  tp () ; 
  ps ".field public " ;
  ifnn (List.iter (ms fieldattribute)) fd.fatt ;
  iltype fd.fsig ;
  ps " " ; ident fd.fnme ;  pnl ()

and ilmethod m = 
  tp () ; ps ".method " ;
  ifnn (List.iter (ms methodattribute)) m.matt ;
  callconv (if List.mem MAstatic m.matt then NoInstance else Instance) ;
  iltype m.mtyp ; ps " " ;
  ident m.mnme ; 
  signature m.mprm ;
  ifnn (List.iter (ms implementattribute)) m.miat ;
  pnl () ;
  tp () ; pel "{" ; tab () ;
  tp () ; pf ".maxstack %d\n" 
     ( match m.maxstack with None -> m.mcin.maxs | Some x -> x) ;
  ifnn (fun x -> tp () ; ps ".locals init " ; signature x ; pnl ()) m.locals ;
  ( if m.entrypoint then ( tp () ; pel ".entrypoint" )) ;
  ifnn (fun x -> tab () ; code x ; untab ()) m.minst ;
  untab (); tp () ; ps "} // end of method" ; pnl(); pnl ()
 
