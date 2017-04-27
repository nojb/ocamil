(***********************************************************************)
(*                                                                     *)
(*                                CamIL                                *)
(*                                                                     *)
(*            Bruno Pagano, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(***********************************************************************)

exception Bug_Report of string * string 
let bug loc mes = raise (Bug_Report (loc,mes))

let not_mem l x = not (List.mem x l) 

let rec union l1 = function 
    [] -> l1 
  | a::l2 -> if List.mem a l1 then union l1 l2 else union (a::l1) l2 

let make_list f n = 
  let rec ml = function 
      p when p=n -> [f p]
    | p -> let q = (f p) in q::(ml (p+1)) 
  in if n<=0 then [] else (ml 1)

let rec sub_list n l = match (n,l) with 
    0,_ -> []
  | n,a::l -> a::(sub_list (n-1) l)
  | _,_ -> failwith "Utils.sub_list" 

let prefix_list n l = sub_list ((List.length l)-n) l 

let rec cut_list n l =  match (n,l) with 
    0,_ -> [],l
  | n,a::l -> let (x,y) = cut_list (n-1) l in (a::x,y) 
  | _,_ -> failwith "Utils.cut_list" 

let last_list l = match cut_list ((List.length l)-1) l with 
    l,[a] -> l,a 
  | _,_ -> failwith "Utils.last_list" 

let rec tail_list = function 
    [] -> invalid_arg "Utils.tail_list" 
  | [a] -> a
  | _::l -> tail_list l 

let array_find f a = 
  let l = Array.length a in
  if l=0 then raise Not_found 
  else 
    let i = ref 0 in 
    while not (f a.(!i)) do if !i>=l then raise Not_found else incr i done ;
    a.(!i)

let rec list_pos_pred pred = function
    [] -> raise Not_found 
  | a::_ when pred a -> 0
  | _::l -> (list_pos_pred pred l) + 1 

let list_pos x l = list_pos_pred ((=) x) l

let rec iter_except_last f1 f2 = function
    [] -> bug "Utils" "iter_except_last"
  | [a] -> f1 a 
  | a::l -> f1 a ; f2 () ; iter_except_last f1 f2 l 


let rec filter_map p f = function 
    [] -> [] 
  | a::l -> if p a then let b = f a in b :: (filter_map p f l) 
            else filter_map p f l 

let someof = function Some x -> x | None -> bug "Utils" "someof" 

(* decomposes "A.B.C.t" to ["t";"C";"B";"A"] *)
let rec decompose_pathname paname =
  try
    let i = String.rindex paname '.' in
    let paname2 = String.sub paname 0 i
    in (String.sub paname (i+1) ((String.length paname)-i-1))::(decompose_pathname paname2)
  with Not_found -> [paname] 

(* forward declaration in Compilenv *)
let current_unit_name = ref (fun () -> "")
