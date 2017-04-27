(* abstract Caml type carrying CLI exception *)
type cli_exception

(* Caml exception fired by non-Caml managed IL exception *)
exception ManagedException of string * cli_exception;;

external getCLIExceptionName: cli_exception -> string =
 "string" "CamIL.Exception" "getCLIExceptionName" "object"

external getCLIinnerException: cli_exception -> cli_exception =
 "object" "CamIL.Exception" "getCLIinnerException" "object"

let getInnerException e = 
  let e' = getCLIinnerException e in 
    (getCLIExceptionName e,e)

external getMessage: cli_exception -> string =
 "string" "CamIL.Exception" "getCLIExceptionMessage" "object"

