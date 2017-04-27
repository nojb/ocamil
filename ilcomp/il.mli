(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)


(* idents *)

type id = string 
type nsid = string 
type label = string 

(* Attributes for classes, methods, etc ... *)

type classattribute = 
    CAnestedassembly | CAnestedfamandassem | CAnestedfamily | CAnestedfamorassem 
  | CAnestedprivate  | CAnestedpublic      | CAprivate      | CApublic
  | CAauto | CAexplicit | CAsequential
  | CAinterface
  | CAabstract | CAsealed 
  | CAansi | CAautochar | CAunicode
  | CAbeforefieldinit | CAserializable | CAspecialname | CArtspecialname (* | CAhassecurity *)


type fieldattribute = 
    FAprivate | FAfamandassem | FAassembly | FAfamily | FAfamorassem 
  | FApublic  | FAstatic | FAinitonly | FAliteral | FAnotserialized 
  | FAspecialname | FApinvokeimpl
  | FArtspecialname | FAhasfieldmarshal | FAhasdefault | FAhasfieldRVA

type methodattribute = 
    MAprivate | MAfamandassem | MAassembly | MAfamily | MAfamorassem 
  | MApublic  | MAstatic | MAfinal | MAvirtual | MAhidebysig
  | MAabstract | MAspecialname
  | MApinvokeimpl
  | MArtspecialname   | MAhassecurity | MArequiresecobject 

type callingconvention = Instance | NoInstance

type implementattribute =
    IAil | IAnative | IAoptil | IAruntime
  | IAunmanaged | IAmanaged
  | IAforwardref | IApreservesig | IAsynchronized | IAnoinlining

(* metadata *) 


type assembly =
    {
      aver:string;
      aflg:bool;    (*currently only holds the PuclicKey flag *)
      akey:string;
      anme:string;
      amod:string;
    }


type assemblyref =
    {
      arver:string;
      arflg:bool;    (*currently only holds the PuclicKey flag *)
      arkey:string;
      arnme:string;
      arhsh:string;
    }

type assemblyscope =
  | ExternalAssembly of assemblyref
  | ThisAssembly
  | UnknownAssembly

type elementType = 
  | Tvoid | Tbool | Tchar   
  | Tint8   | Tint16  | Tint32  | Tint64
  | Tuint8  | Tuint16 | Tuint32 | Tuint64
  | Tfloat32 | Tfloat64 
  | Tnint | Tnuint 
  | Tstar of elementType
  | Tampersand of elementType 
  | Tvector of elementType
  | Tvaluetype of typeref
  | Tclass of typeref
  | Tstring 
  | Tobject
 (* missing: *)
 (* | Tboxed of  *)
 (* | Tfnptr *)
 (* | Ttypedref *)
 (* | Tmodifiers *)

and signature = (elementType * id option) list 


(* in metadata, fieldref and methodref are mixed in memberref *)

and typedef = 
  { 
    mutable tdnme : id ;                  (* name *)
    mutable tdnsp : nsid ;                (* namespace *)
    mutable tdatt : classattribute list ; (* attributes *)
    mutable tdext : typeref option;       (* extends *)
    mutable tdimp : typeref list ;        (* implements *)
    mutable tdfld : field list ;          (* fields *)
    mutable tdmet : methode list ;        (* methods *)
(*    mutable tdncl : typeref list ;      (* nested classes *) *)
(*    mutable trcus : customdecl list ;   (* custom attributes *) *)
  }

and typeref = 
    {
      mutable trscp: assemblyscope;
      mutable trnme: id;
      mutable trnsp: nsid;
    }

and field = 
  { 
    mutable fnme : id ;                (* name *)
    mutable fsig : elementType ;       (* type *)
    mutable fatt : fieldattribute list (* attributes *)
  }

and fieldref =
    {
      frcla: typeref;
      frnme: id;
      mutable frsig: elementType;
    }

and methode = 
  { 
    (*mutable mprt : ? ;                     (* parent class *) *)
    mutable mnme : id ;                      (* name *)
    mutable matt : methodattribute list ;    (* attribute *)
    mutable miat : implementattribute list ; (* attribute (implementation) *)
    (* the 2 following fields shall be blended in a blob's signature *)
    mutable mprm : signature ;               (* argument's types *)
    mutable mtyp : elementType ;             (* return type *)
(*    mutable ccnv : callconv ;              (* calling convention *) *)
    mutable maxstack : int option ;          (* size max of stack *)
    mutable locals : signature ;             (* local variables *)
    mutable entrypoint : bool ;              (* is it the entry point ? *)
    mutable minst : instruction list ;       (* code *)
    mutable mcin : comp_info                 (* temporary information *)
  }

and methodref = 
    {
      mrcla: typeref;
      mrnme: id;
      mrprm: signature;
      mrtyp: elementType;
      mutable mrccn: callingconvention;
    }

and typerefortypespec =
  | TypeRef of typeref | TypeSpec of elementType

and instruction = 
  | Icomment of string 
  | Ilabel of label 
  | Itry_main
  | Itry_catch
  | Itry_end

  | Iadd 
  | Iand
  | Ibeq of label 
  | Ibge of label 
  | Ibgt of label 
  | Ible of label 
  | Iblt of label 
  | Ibox of typerefortypespec
  | Ibr of label 
  | Ibrtrue of label 
  | Ibrzero of label 
  | Icall of methodref
  | Icallvirt of methodref

  | Iconvi4
  | Iconvi8
  | Iconvi

  | Icastclass of typerefortypespec
  | Iceq 
  | Icgt  
  | Iclt
  | Icgt_un  
  | Iclt_un
  | Idiv
  | Idup
  | Ildarg of int 
  | Ildci4 of int 
  | Ildci8 of int64
  | Ildcr8 of float
  | Ildindi1
  | Ildindi2
  | Ildindi4
  | Ildindi8
  | Ildindi
  | Ildindr8
  | Ildelem of string 
  | Ildfld of fieldref
  | Ildlen 
  | Ildloc of int  
  | Ildnull
  | Ildsfld of fieldref
  | Ildstr of string  
  | Ileave of label
  | Imul
  | Ineg 
  | Inewarr of typerefortypespec
  | Inewobj of methodref
  | Inot
  | Ior
  | Ipop
  | Irem  
  | Iret 
  | Ishl
  | Ishr
  | Ishrun
  | Istelem of string
  | Istfld of fieldref
  | Istloc of int 
  | Istarg of int 
  | Istsfld of fieldref
  | Isub 
  | Iswitch of label list 
  | Itail 
  | Ithrow
  | Iunbox of typerefortypespec
  | Ixor
  | Iisinst of typerefortypespec
  | Irethrow

(* types for structure *)



(* the compiling information is used when generating il code 
   of a method :
    - locfree ans locassc are used to optimize the local 
       variables.
     - stack infos are needed to :
           * specify the max size stack 
           * dump and restore the stack before and after a try-catch block 
     - labels : label counter *)

and comp_info = 
  { mutable locfree : (int * elementType) list ; (* local vars not used *)
    mutable locassc : (id * id) list ;           (* mapping of local vars *)
    mutable register : int ;                     (* for new locals *)
    mutable maxs : int ;                         (* max stack size *)          
    mutable curs : int ;                         (* current stack size *)
    mutable stck : elementType list ;            (* what contains the stack ? *)
    mutable labels : int }                       (* count labels *)



type il_compilation_unit =
    {
      mutable icuass:assembly option;
      mutable icuassref:assemblyref list;
      mutable icutypes:typedef list;
    }

type local_id = methodref * id

type where_return = WRcont | WRret | WRbr of label | WRlv of label

(* bool = true means the cast is irreductible *)
type castsort = 
    InsertCast of bool * (methode -> unit) 
  | InsertCastWR of bool * (methode -> where_return -> unit) 
  | NoCast

