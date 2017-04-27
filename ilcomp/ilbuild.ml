(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

open Il

let ref_of_class cl = {trscp=UnknownAssembly;trnme=cl.tdnme;trnsp=cl.tdnsp}

let ref_of_method mt cla = {mrcla=cla;mrnme=mt.mnme;mrprm=mt.mprm;mrtyp=mt.mtyp;
			    mrccn=if List.mem MAstatic mt.matt then NoInstance else Instance}

let ref_of_field fd cla = {frcla=cla;frnme=fd.fnme;frsig=fd.fsig}

let class_of icu ns name = List.find (fun x -> x.tdnsp=ns && x.tdnme=name) icu.icutypes

let method_of cl name = 
  try List.find (fun x -> x.mnme=name) cl.tdmet
  with Not_found -> failwith ("Ilbuil.method_of : " ^ cl.tdnme ^ "::" ^ name)

let field_of cl name  = 
  try List.find (fun x -> x.fnme=name) cl.tdfld
  with Not_found -> failwith ("Ilbuild.field_of : " ^ cl.tdnme ^ "::" ^ name)

(* create empty structures of IL *)

let new_class icu ns id =
  let c = { tdnme = id ; tdnsp = ns ; tdatt=[]; tdext = None ;
            tdimp=[]; tdfld=[]; tdmet=[] } in 
    icu.icutypes <- icu.icutypes @ [c] ;
  c

let new_field cl id it =  
  let f = { fnme=id ; fsig=it ; fatt=[] } in
  cl.tdfld <- cl.tdfld @ [f] ;
  f 

let new_comp_info () = 
  { locfree=[]; locassc=[]; register=0; maxs=0; curs=0; stck=[]; labels=0 } 

let new_icu () =
  {icuass=None;icuassref=[];icutypes=[]}

let new_method prt id tp si = 
  let m =  
    { 
      mnme = id ;
      matt=[MApublic]; miat=[];       (* !! *)
      mprm = si ; mtyp = tp ;
      maxstack = None ;
      locals=[]; 
      entrypoint=false;
      minst=[];  
      mcin = new_comp_info () } in
    prt.tdmet <- prt.tdmet @ [m] ;
    m

let new_ctor prt si = 
  let m = new_method prt ".ctor" Tvoid si in 
    m.matt <- [MApublic; MAspecialname; MArtspecialname];
    m.miat <- [IAmanaged; IAil] ;
    m    
      
(* modify a class *)

let add_catt c att = if not (List.mem att c.tdatt) then c.tdatt <- att::c.tdatt 
let add_extd c cf = c.tdext <- Some cf 
let add_cfld cl id it = ignore (new_field cl id it) 
let add_sfld cl id it = 
  let f = new_field cl id it in 
    f.fatt <- FAstatic::f.fatt


(* modify a field *)

let add_fatt f att = if not (List.mem att f.fatt) then f.fatt <- att::f.fatt

(* modify a method *)

let add_matt m att = if not (List.mem att m.matt) then m.matt <- att::m.matt 
let add_miat m att = if not (List.mem att m.miat) then m.miat <- att::m.miat 
let add_inst m i = m.minst <- i :: m.minst 
let add_locals m id typ = m.locals <- m.locals @ [typ,Some id] 

(* miscellaneous *)

let init_label m =
  "label_" ^ m.mnme ^ "_init"

let new_label m = 
  let q = m.mcin.labels in 
  m.mcin.labels <- m.mcin.labels + 1 ;
    "label_" ^ m.mnme ^ "_" ^ (string_of_int q)

let real_loc m id = 
  try  List.assoc id m.mcin.locassc 
  with Not_found -> id 

let rec release_loc m id = 
  try 
    let pos = Utils.list_pos_pred (fun (_,y) -> y=Some id) m.locals in
    let (typ,_) = List.nth m.locals pos in 
    m.mcin.locfree <- (pos,typ)::m.mcin.locfree 
  with Not_found ->
    let rid = List.assoc id m.mcin.locassc in
    m.mcin.locassc <- List.filter (fun (x,y) -> x<>id) m.mcin.locassc ;
    release_loc m rid 

let assoc_loc m id pos =
  m.mcin.locfree <- List.filter (fun (x,_) -> x<>pos) m.mcin.locfree ;
  let rid = Utils.someof (snd (List.nth m.locals pos)) in 
  m.mcin.locassc <- (id,rid) :: m.mcin.locassc 

let create_local m id typ = (* no mli *)
  try 
    let (pos,_) = List.find (fun (_,x) -> x=typ) m.mcin.locfree in 
    assoc_loc m id pos 
  with Not_found -> 
    add_locals m id typ 

let new_register m typ = 
  let id = "reg_" ^ (string_of_int m.mcin.register ) in 
  m.mcin.register <- m.mcin.register + 1 ;
  create_local m id typ ;
  id 


let type_ref nsid id =
  { trscp = UnknownAssembly;
    trnme = id;
    trnsp = nsid; }

let method_ref cc prt id typ si =
  {
    mrcla=prt;
    mrnme=id;
    mrprm=si;
    mrtyp=typ;
    mrccn=cc;
  }

let ctor_ref prt si = method_ref Instance prt ".ctor"  Tvoid si

let field_ref prt id typ =
  {
    frcla=prt;
    frnme=id;
    frsig=typ;
  }

let typeref_of_typedef t = type_ref t.tdnsp t.tdnme
let fieldref_of_fielddef cl f = field_ref (typeref_of_typedef cl) f.fnme f.fsig
let methodref_of_methode cl met = 
  method_ref (if List.mem MAstatic met.matt then NoInstance else Instance)
    (typeref_of_typedef cl) met.mnme met.mtyp met.mprm
