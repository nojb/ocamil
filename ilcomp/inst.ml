(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)


open Il
open Ilbuild 

(* stack handling *)

let push m x = m.mcin.stck <- x :: m.mcin.stck ;
               m.mcin.curs <- m.mcin.curs + 1 ;
               m.mcin.maxs <- max m.mcin.maxs m.mcin.curs

let spop m = 
  try  m.mcin.stck <- List.tl m.mcin.stck ;
       m.mcin.curs <- m.mcin.curs - 1
  with Failure "tl" -> 
      let cui=Compilenv.get_current_unit() in
	Utils.bug "Inst.pop" ((cui.Compilenv.ui_class).trnme^"::"^m.mnme)

(*
let push m x =  add_inst m (Icomment "push") ; push m x 
let spop m = add_inst m (Icomment "pop") ;try spop m with _ -> add_inst m (Icomment "pop - problem" )
*)

let nspop m n = for i=1 to n do spop m done (*no mli*)

let change_stack m t = spop m ; push m t 
let force_stack m t =  m.mcin.stck <- [t] ; m.mcin.curs <- 1 

let dump_stack m = (m.mcin.stck,m.mcin.curs)
let restore_stack m (x,y) = m.mcin.stck <- x ; m.mcin.curs <- y (*no mli*)

(* locals handling *)
(* real_loc, release_loc and assoc_loc defined in ilbuild *)

                         (** *************** **)

(* "meta" instructions *)

let comment m str = 
  add_inst m (Icomment str) 

let label m (lbl,(st,cu)) = 
  add_inst m (Ilabel lbl) ;
  if st<>m.mcin.stck || cu<>m.mcin.curs then Utils.bug "Inst.label" lbl
(*  then add_inst m (Icomment ("label " ^ lbl))  *)

let label_force m (lbl,(st,cu)) = 
  restore_stack m (st,cu) ;
  add_inst m (Ilabel lbl) 

let label_follow m lbl = 
  add_inst m (Ilabel lbl) 

let try_main m = 
  if m.mcin.curs>0 then Utils.bug "Inst.register_stack" "stack not empty" ;
  add_inst m Itry_main

let try_catch m = 
  add_inst m Itry_catch

let try_end m = 
  add_inst m Itry_end 

(* simple instructions, without token *)

let add m = 
  add_inst m Iadd ;
  spop m 

let _and m = 
  add_inst m Iand ;
  spop m 

let beq m lbl = 
  add_inst m (Ibeq lbl) ; 
  nspop m 2 ;
  (lbl,dump_stack m)

let bgt m lbl = 
  add_inst m (Ibgt lbl) ; 
  nspop m 2 ;
  (lbl,dump_stack m)

let blt m lbl = 
  add_inst m (Iblt lbl) ; 
  nspop m 2 ;
  (lbl,dump_stack m)

let bge m lbl = 
  add_inst m (Ibge lbl) ; 
  nspop m 2 ;
  (lbl,dump_stack m)

let ble m lbl = 
  add_inst m (Ible lbl) ; 
  nspop m 2 ;
  (lbl,dump_stack m)

let br m lbl = 
   add_inst m (Ibr lbl) ; 
  (lbl,dump_stack m)

let brfalse m lbl = 
  add_inst m (Ibrzero lbl) ; 
  spop m ;
  (lbl,dump_stack m)

let brtrue m lbl = 
  add_inst m (Ibrtrue lbl) ; 
  spop m ;
  (lbl,dump_stack m)

let convi4 m  =
  add_inst m (Iconvi4) ;
  spop m ; 
  push m Ilpredef.int32_type

let convi8 m  =
  add_inst m (Iconvi8) ;
  spop m ; 
  push m Ilpredef.int64_type

let convi m  =
  add_inst m (Iconvi) ;
  spop m ; 
  push m Ilpredef.nint_type

let ceq m = 
  add_inst m (Iceq) ;
  spop m 

let cgt m = 
  add_inst m (Icgt) ;
  spop m 

let clt m = 
  add_inst m (Iclt) ;
  spop m 

let cgt_un m = 
  add_inst m (Icgt_un) ;
  spop m 

let clt_un m = 
  add_inst m (Iclt_un) ;
  spop m 

let div m = 
  add_inst m Idiv ;
  spop m 

let dup m = 
  add_inst m Idup ;
  let stckhd = 
    try List.hd m.mcin.stck  
    with Failure "hd" -> 
      let cui=Compilenv.get_current_unit() in
	Utils.bug "Inst.dup" ((cui.Compilenv.ui_class).trnme^"::"^m.mnme)
  in push m stckhd

let ldarg m n =
  try 
    add_inst m (Ildarg n) ;
    let t = match (List.mem MAstatic m.matt) , n with 
        false , 0 -> let cui=Compilenv.get_current_unit() in
	  Tclass (cui.Compilenv.ui_class)
      | false , _ -> fst (List.nth m.mprm (n-1))
      | true , _ -> fst (List.nth m.mprm n)
    in push m t
  with Failure "nth" ->
    let cui=Compilenv.get_current_unit() in
      Utils.bug "Inst.ldarg" ((cui.Compilenv.ui_class).trnme^"::"^m.mnme^"."^(string_of_int n))


let ldci4 m n = 
  add_inst m (Ildci4 n) ; 
  push m Tint32

let ldci8 m n = 
  add_inst m (Ildci8 n) ; 
  push m Tint64

let ldcr8 m x = 
  add_inst m (Ildcr8 x) ;
  push m Tfloat64 


let ldindi1 m =
  add_inst m  Ildindi1;
  spop m;
  push m Ilpredef.int8_type

let ldindi2 m =
  add_inst m  Ildindi2;
  spop m;
  push m Ilpredef.int16_type

let ldindi4 m =
  add_inst m  Ildindi4;
  spop m;
  push m Ilpredef.int32_type

let ldindi8 m =
  add_inst m  Ildindi8;
  spop m;
  push m Ilpredef.int64_type

let ldindi m =
  add_inst m  Ildindi;
  spop m;
  push m Ilpredef.nint_type

let ldindr8 m =
  add_inst m  Ildindr8;
  spop m;
  push m Ilpredef.float64_type

let ldelem_ref m = 
  add_inst m (Ildelem "ref") ;
  nspop m 2 ;
  push m Ilpredef.object_type 
  
let ldelem_char m =
  add_inst m (Ildelem "i2") ;
  nspop m 2;
  push m Ilpredef.char_type

let ldlen m = 
  add_inst m Ildlen ;
  spop m ;
  push m Tint32 

let ldloc m n t = 
  add_inst m (Ildloc n) ;
  push m t

let ldnull ?(of_type=Ilpredef.object_type) m = 
  add_inst m Ildnull ;
  push m of_type

let ldstr m s = 
    add_inst m (Ildstr s) ;
    push m Ilpredef.string_type 

let leave m lbl = 
  add_inst m (Ileave lbl) ; 
  (lbl,dump_stack m)

let mul m = 
  add_inst m Imul ;
  spop m 

let neg m = 
  add_inst m Ineg 

let _or m = 
  add_inst m Ior ;
  spop m 

let pop m = 
  add_inst m Ipop ;
  spop m 

let rem m = 
  add_inst m Irem ;
  spop m 

let ret m = 
  add_inst m Iret ;
  if m.mtyp<>Tvoid then spop m ;
(*  if m.mcin.curs<>0 then Utils.bug "Inst.ret" (m.mprt.cnme^"::"^m.mnme) *)
  if m.mcin.curs<>0 then add_inst m (Icomment "ret prob")

let rethrow m =
  add_inst m Irethrow 
  
let shl m = 
  add_inst m Ishl ;
  spop m 
                         
let shr m = 
  add_inst m Ishr ;
  spop m 

let shr_un m = 
  add_inst m Ishrun ;
  spop m 
                                                  
let stelem_ref m = 
  add_inst m (Istelem "ref") ; 
  nspop m 3

let stelem_char m =
  add_inst m (Istelem "i2") ;
  nspop m 3

let stloc m n = 
  add_inst m (Istloc n) ;
  spop m     

let starg m n =
  add_inst m (Istarg n) ;
  spop m

let switch m lbls =
  add_inst m (Iswitch lbls) ;
  spop m; 
  let ds = dump_stack m in
  List.map (fun x -> (x,ds)) lbls 

let sub m = 
  add_inst m Isub ;
  spop m 

let tail m = 
  add_inst m Itail 

let _xor m = 
  add_inst m Ixor ;
  spop m 


(* complex instructions, with token *)

let box m t = 
  add_inst m (Ibox (TypeRef t)) ;
  spop m ;
  push m Ilpredef.object_type  

let castclass m t =
  if not !Clflags.unverif then add_inst m (Icastclass (TypeRef t)) ;
  spop m ; 
  push m (Tclass t)

let newarr m t = 
  add_inst m (Inewarr (TypeRef t)) ;
  spop m ; push m (Tvector (Tclass t)) 

let unbox m t = 
  add_inst m (Iunbox (TypeRef t)) ;
  spop m ;
  push m (Tclass t)  

let isinst m t = 
  add_inst m (Iisinst (TypeRef t)) ;
  spop m ;
  push m (Tclass t)  

let box_spec m t = 
  add_inst m (Ibox (TypeSpec t)) ;
  spop m ;
  push m Ilpredef.object_type  

let castclass_spec m t =
  if not !Clflags.unverif then add_inst m (Icastclass (TypeSpec t)) ;
  spop m ; 
  push m t

let newarr_spec m t = 
  add_inst m (Inewarr (TypeSpec t)) ;
  spop m ; push m (Tvector t) 

let unbox_spec m t = 
  add_inst m (Iunbox (TypeSpec t)) ;
  spop m ;
  push m t

let isinst_spec m t = 
  add_inst m (Iisinst (TypeSpec t)) ;
  spop m ;
  push m t


let call m cla mn rt args inst =
  let methref = Ilbuild.method_ref inst cla mn rt args in
    add_inst m (Icall methref) ;
    nspop m (List.length args) ; 
    if inst=Instance then spop m ;
(* special pour la seule reference connue a Tclass "System.Object" au lieu de Tobject a cause du constructeur de object dans Default *)
    match rt with Tclass tr when tr.trnsp="System" && tr.trnme = "Object" ->
      push m Tobject
      | _ -> ();

    if rt<>Tvoid then push m rt 
    
let callvirt m cla mn rt args =
  let methref = Ilbuild.method_ref Instance cla mn rt args in
    add_inst m (Icallvirt methref) ;
    nspop m (List.length args) ; spop m ;
(* special pour la seule reference connue a Tclass "System.Object" au lieu de Tobject a cause du constructeur de object dans Default *)
    match rt with Tclass tr when tr.trnsp="System" && tr.trnme = "Object" ->
      push m Tobject
      | _ -> ();

    if rt<>Tvoid then push m rt 

let newobj m cla mn rt args (*cltyp*) = (* !! bizarre mn = ".ctor" non ? *)
  let cltyp = Tclass cla in
  let methref = Ilbuild.method_ref Instance cla mn rt args in
    add_inst m (Inewobj methref) ;
    nspop m (List.length args) ;
    push m cltyp;
(* special pour la seule reference connue a Tclass "System.Object" au lieu de Tobject a cause du constructeur de object dans Default *)
    match rt with Tclass tr when tr.trnsp="System" && tr.trnme = "Object" ->
      push m Tobject
      | _ -> ();

    if rt<>Tvoid then push m rt 



let ldfld m fr = 
  add_inst m (Ildfld fr) ;
  spop m ;
  push m fr.frsig

let ldsfld m fr = 
  add_inst m (Ildsfld fr) ;
  push m fr.frsig 

let stfld m fr  = 
  add_inst m (Istfld fr) ;
  nspop m 2 

let stsfld m fr = 
  add_inst m (Istsfld fr) ;
  spop m  


                         (** *************** **)

let call_method m met = 
  call m met.mrcla met.mrnme met.mrtyp met.mrprm met.mrccn
(*  call m (name_of_method met) met.mtyp met.msig (met.ccnv=Instance)  *)

let callvirt_method m met = 
  callvirt m met.mrcla met.mrnme met.mrtyp met.mrprm
(*  callvirt m (name_of_method met) met.mtyp met.msig  *)

let newobj_ctor m ctor =
  newobj m ctor.mrcla ctor.mrnme ctor.mrtyp ctor.mrprm

let ldarg_name m id =
  try 
    let is_loc x = match x with (_,Some n) -> n=id | _ -> false in
    let var = List.find is_loc m.mprm in 
    let pos = Utils.list_pos_pred is_loc m.mprm in 
      if List.mem MAstatic m.matt then ldarg m pos 
      else ldarg m (pos+1)
  with Not_found -> Utils.bug "Inst.ldarg_name" id

let ldfld_field m fr inst =
  if inst=NoInstance then ldsfld m fr else ldfld m fr

let ldloc_name m id = 
  try 
    let name = real_loc m id in 
    let is_loc x = match x with (_,Some n) -> n=name | _ -> false in
    let var = List.find is_loc m.locals in 
    let pos = Utils.list_pos_pred is_loc m.locals in 
    ldloc m pos (fst var) 
  with Not_found -> 
    prerr_string ("Warning (Inst) : unfound local "^id^"\n");
    flush stderr;
    () (*00 si c'est bien un local void, rien a faire ... *)
    (*00 Utils.bug "Inst.ldloc_name" id*)

let newobj_class mt cl = 
  try
    let m = List.find (fun x -> x.mnme=".ctor") cl.tdmet in
    newobj mt (typeref_of_typedef cl) m.mnme m.mtyp m.mprm
  with Not_found -> 
    Utils.bug "Inst.newobj_class" cl.tdnme

let newobj_class_with_size mt cl size = 
  try
    let m = List.find (fun x -> x.mnme=".ctor" && (List.length x.mprm)=size) cl.tdmet in
    newobj mt (typeref_of_typedef cl) m.mnme m.mtyp m.mprm
  with Not_found -> 
    Utils.bug "Inst.newobj_class_with_size " cl.tdnme

let stfld_field m fr inst =
  if inst=NoInstance then stsfld m fr else stfld m fr


                         (** *************** **)
  

let stloc_name m id = 
  try 
    let name = real_loc m id in 
    let is_loc x = match x with (_,Some n) -> n=name | _ -> false in
    let pos = Utils.list_pos_pred is_loc m.locals in 
    stloc m pos 
  with Not_found -> Utils.bug "Inst" "stloc"

let new_local m id typ =
  try 
    let (pos,_) = List.find (fun (_,x) -> x=typ ) m.mcin.locfree in 
    assoc_loc m id pos 
  with Not_found -> 
    add_locals m id typ 

(* rem : new_register and new_label in ilbuild *)

let register_stack m = 
  let regs = List.map (new_register m) m.mcin.stck in
  List.iter (stloc_name m) regs ;
  List.rev regs 

let stack_register m regs = 
  List.iter (ldloc_name m) regs ;
  List.iter (release_loc m) regs  



                         (** *************** **)

(* AJOUTS RAF *)

let lbl_st_rest = ref ([] : (Il.label * (string list)) list);; (*TODO chger string en truc id *)

let remember_lbl_st_restore m lbl =
  try 
    let prevstack = List.assoc lbl !lbl_st_rest in
      (* lors du deuxième appel, en compilant le bloc catch *)
      if (m.mcin.curs != List.length prevstack) then (*add_inst m (Icomment "stack inconsistency - problem");*) Utils.bug "Inst.rmbr" "Piles en sortie de try et catch de tailles differentes";
      if m.mcin.curs>0 then List.iter (fun id -> stloc_name m id) (List.rev prevstack);
  with Not_found -> (* lors du premier appel, en compilant le bloc try *)
    let stack =  
      if m.mcin.curs>0 then register_stack m  (* cet appel engendre les stloc par effet de bord *)
      else [] 
    in 
      lbl_st_rest := (lbl, stack) :: !lbl_st_rest
	
let execute_lbl_st_restore m lbl =
  try 
    let idl=List.assoc lbl !lbl_st_rest in
      lbl_st_rest := List.remove_assoc lbl !lbl_st_rest;
      stack_register m idl
  with Not_found -> Utils.bug "Compil.tlambda de RAF" "label absent de la table de restauration"


let br_for_staticfail m lbl =
  add_inst m (Ibr lbl) ; 
  push m Ilpredef.object_type  (* juste pour la coherence des chemins d'inspection de pile *)


let rec compose_casts c1 c2 = match c1,c2 with
    NoCast,_ -> c2
  | _,NoCast -> c1
  | InsertCast (irr1,f1),InsertCast (irr2,f2) -> InsertCast (irr1 or irr2,fun mt -> f1 mt;f2 mt)
  | InsertCast (irr1,f1),InsertCastWR (irr2,f2) -> InsertCastWR (irr1 or irr2,fun mt wr -> f1 mt;f2 mt wr)
  | InsertCastWR (irr1,f1),InsertCastWR (irr2,f2) -> InsertCastWR (irr1 or irr2,fun mt wr -> f1 mt wr;f2 mt wr)
  | InsertCastWR (irr1,f1),InsertCast (irr2,f2) -> InsertCastWR (irr1 or irr2,fun mt wr -> f1 mt wr;f2 mt)

let castsort_implicit irr typ = InsertCast (irr,(fun mt -> change_stack mt typ))
  
