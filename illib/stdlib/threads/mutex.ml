type t

external mutex_create: string -> t =
"class System.Threading.Mutex" "CamIL.Threads" "mutex_create" "string"

(* TODO faire un generateur d'IDs fraiches *)
let last_nb = ref 0;;

let create () =
  incr last_nb;
  let name = string_of_int(!last_nb) in
  mutex_create name
    
external mutex_waitone: t -> int -> bool =
"int" "CamIL.Threads" "mutex_waitone" "class System.Threading.Mutex" "int"

let lock m =
  let _=mutex_waitone m (-1) in ()

let try_lock m =
  mutex_waitone m 1000

external mutex_release: t -> unit =
"void" "CamIL.Threads" "mutex_release" "class System.Threading.Mutex"

let unlock m =
  let _=mutex_release m in ()

