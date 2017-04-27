(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

let core_camil_assbly = "core_camil"

(* quoting functions *)
let subst_quote s = 
  let res = String.create (String.length s) in
  for i=0 to (String.length s)-1 do 
    res.[i] <- (if s.[i]<>'\'' then s.[i] else '"')
    done ;
  res  

let quote s = "'" ^ (subst_quote s) ^ "'"

let quote_qualified s = 
  let bits = Utils.decompose_pathname s in
    List.fold_left (fun s chunk -> if s="" then quote chunk else (quote chunk)^"."^s)
      "" bits

let unbracket s = 
  let l = String.length s in
    if l>1 && s.[0]='[' && s.[l-1]=']' then String.sub s 1 (l-2) else s 

(* generate a symbol satisfying a predicate *)
let gensym p s =
  if (p s) then s
  else
    let v = ref (s ^ "_0") in
    let i = ref 1 in 
    while not (p !v) do v:= s ^ "_" ^ (string_of_int !i) ; incr i  done ;
    !v

(* local name table type *)
type name_table = 
  { mutable unit_id : Il.id ; 
    mutable classes : Il.typeref list ;
    mutable fields : (Il.typeref * Il.id) list ;
    mutable locals : ((Il.typeref * Il.id) * Il.id) list
      
  }


let name_tbl = { unit_id="" ; classes=[] ; fields=[] ; locals=[]}

(* init local name table *)
(* = naming namespace of current unit *)
let init_name_table id_unit = 
  let cui = Compilenv.get_current_unit () in
  let un = 
    let units = List.map (fun (_,x,_) -> x) cui.Compilenv.ui_imports_cmx in
    let pred x = List.for_all (fun u -> u.Il.trnme <> "Top" || u.Il.trnsp <>x) units in 
    gensym pred id_unit in 
    cui.Compilenv.ui_class <- Ilbuild.type_ref un "Top" ;
    name_tbl.unit_id <- un ;
    name_tbl.classes <- [] ; 
    name_tbl.fields <- [] ;
    name_tbl.locals <- [] 


(* return the local unit name *)
let get_unit_id () = name_tbl.unit_id

(* create a new class name *)
let new_class_name namespace base = 
  let namespace = (get_unit_id())^"."^namespace in
  let res = gensym (fun s -> List.for_all (fun tr ->  tr.Il.trnsp <> namespace || tr.Il.trnme <> s) name_tbl.classes) base in 
    name_tbl.classes <- (Ilbuild.type_ref namespace res) :: name_tbl.classes;
    res

(* a function label has the generic form "module__ident_nnn" *)
(* cut_label extract "ident" if it is possible               *)
let cut_label label = 
  let l1 = String.length label 
  and l2 = String.length name_tbl.unit_id in 
  if l2+1 < l1 && label.[l2] = '_' && (String.sub label 0 l2) = name_tbl.unit_id
  then 
    let i = ref (l1-1) in 
    while label.[!i] >= '0' && label.[!i] <= '9' do decr i done ;
    if label.[!i] = '_' then decr i ;
    String.sub label (l2+1) (!i-l2)
  else
    label 

let new_func_label label = 
  let base = cut_label label.Clambda.opt in
  let res = new_class_name "closures" base in (* cannot interfer with namespaces used for ML modules which are uppercase *)
  let labil = { Clambda.ilns = name_tbl.unit_id ^ ".closures" ;  
                Clambda.ilname = res ; 
                Clambda.ilrt = Il.Tvoid ; 
                Clambda.ilsig = [] } in 
  label.Clambda.ilinfo <- Some labil ;
  name_tbl.classes <- (Ilbuild.type_ref (name_tbl.unit_id ^ ".closures" ) res) :: name_tbl.classes 
  

(* create a new name different from others fields of the same class *)
let new_field_name defclass base =
  let pred x = List.for_all (fun (tr,fname) ->  tr.Il.trnsp <> defclass.Il.trnsp || tr.Il.trnme <> defclass.Il.trnme || fname <> x) name_tbl.fields in
  let res =  gensym pred base in
    name_tbl.fields <- (defclass,res) :: name_tbl.fields;
    res
      
(* create a new name different from others locals of the same function *)
let new_local_name defclass funcname base =
  let pred x = List.for_all (fun ((tr,mname),lname) ->  tr <> defclass || mname <> funcname || lname <> x) name_tbl.locals in
  let res =  gensym pred (Ident.name base) in
    name_tbl.locals <- ((defclass,funcname),res) :: name_tbl.locals;
    res


