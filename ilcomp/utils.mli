(***********************************************************************)
(*                                                                     *)
(*                                CamIL                                *)
(*                                                                     *)
(*            Bruno Pagano, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(***********************************************************************)

(* utiliser pour le debug, a supprimer dans la version finale *)
exception Bug_Report of string * string 
val bug : string -> string -> 'a 

(* uniuon of two lists *)
val union : 'a list -> 'a list -> 'a list 

(* not member *)
val not_mem : 'a list -> 'a -> bool

(* make_list f n == [ f 1 ; f 2 ; ... ; f n ] *)
val make_list : (int -> 'a) -> int -> 'a list

(* sub_list n [ l1 ; .. ; lp ] ==  [ l1 ; .. ; ln ] *)
val sub_list : int -> 'a list -> 'a list

(* prefix_list n [ l1 ; .. ; lp ] ==  [ l1 ; .. ; l(n-p) ] *)
val prefix_list : int -> 'a list -> 'a list

(* cut_list  n [ l1 ; .. ; lp ] ==  [ l1 ; .. ; ln ] , [l(n+1) ; .. ; lp ] *)
val cut_list : int -> 'a list -> 'a list * 'a list

(* last_list [ l1 ; .. ; ln ; a ] = [ l1 ; .. ; ln ] , a  *)
val last_list : 'a list -> 'a list * 'a

(* tail_list [...;a] = a *)
val tail_list : 'a list -> 'a 

(* array_find p a : find the first element of a satisfying p *)
val array_find : ('a -> bool) -> 'a array -> 'a

(* list_pos_pred p l : find the pos of the first element of l satisfying p *)
val list_pos_pred : ('a -> bool) -> 'a list -> int

(* list_pos_pred x l : find the pos of the first element of l equal to x *)
val list_pos : 'a -> 'a list -> int

(* iter_except_last f1 f2 [x1..xn;y] -> f1 x1; f2();..;f1 xn;f2 ();f1 y *)
val iter_except_last : ('a -> unit) -> (unit -> unit) -> 'a list -> unit 

(* filter_map p f l : List.map f (List.filter p l) *)
val filter_map : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list

val someof : 'a option -> 'a

(* decomposes "A.B.C.t" to ["t";"C";"B";"A"] *)
val decompose_pathname : string -> string list

val current_unit_name : (unit -> string) ref
