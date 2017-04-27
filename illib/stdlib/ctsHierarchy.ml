(* Top *)
class type cTop =
object
  method _get_ctsobj : Jacare.obj
end

class top (cts_obj:Jacare.obj) =
object
    method _get_ctsobj = cts_obj
end

exception Null_object of string
