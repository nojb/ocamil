(************************************************************************)
(*                                                                      *)
(*                                CamIL                                 *)
(*                                                                      *)
(* created by  Bruno Pagano, projet Cristal, INRIA Rocquencourt (2000)  *)
(* modified by Emmanuel Chailloux & Raphael Montelatici, PPS (2003-2006)*)
(*                                                                      *)
(************************************************************************)

open Format
open Asttypes
open Primitive
open Types
open Lambda
open Typedlambda
open Printlambda


let type_expr_option ppf = function 
  | Some ta -> !Oprint.out_type ppf (Printtyp.tree_of_type_scheme ta.taexpr) (* may print pathes incorrectly, ie in relative form *)
  | None -> fprintf ppf "[??]"

let typedident_print ppf tid =
  fprintf ppf "@[(%a:%a)@]" Ident.print (fst tid) type_expr_option (snd tid)

let rec typed_structured_constant ppf = function
  | TConst_base c -> structured_constant ppf (Const_base c) 
  | TConst_pointer i -> structured_constant ppf (Const_pointer i)
  | TConst_block(tag, []) ->
      fprintf ppf "[%i]" tag
  | TConst_block(tag, (sc1,tann)::scl) ->
      let sconsts ppf scl =
        List.iter (fun (sc,tann) -> fprintf ppf "@ %a:%a" typed_structured_constant sc type_expr_option tann) scl in
      fprintf ppf "@[<1>[%i:@ @[%a:%a%a@]]@]" tag typed_structured_constant sc1 type_expr_option tann sconsts scl
  | TConst_float_array fls -> structured_constant ppf (Const_float_array fls) 

let rec lam ppf tl =
  fprintf ppf "@[(%a,%a)@]" term tl.tlterm type_expr_option tl.tltype

and term ppf = function 
  | TypLvar id ->
      Ident.print ppf id
  | TypLconst cst ->
      typed_structured_constant ppf cst
  | TypLapply(lfun, largs) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | TypLfunction(kind, params, body) ->
      let pr_params ppf params =
        match kind with
        | Curried ->
            List.iter (fun param -> fprintf ppf "@ %a" typedident_print param) params
        | Tupled ->
            fprintf ppf " (";
            let first = ref true in
            List.iter
              (fun param ->
                if !first then first := false else fprintf ppf ",@ ";
                typedident_print ppf param)
              params;
            fprintf ppf ")" in
      fprintf ppf "@[<2>(function%a@ %a)@]" pr_params params lam body
  | TypLlet(str, id, arg, body) ->
      let rec letbody = function
        | {tlterm=TypLlet(str, id, arg, body)} ->
            fprintf ppf "@ @[<2>%a@ %a@]" typedident_print id lam arg;
            letbody body
        | expr -> expr in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" typedident_print id lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | TypLletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" typedident_print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | TypLprim(prim, largs) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | TypLswitch(larg, sw) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
         sw.tsw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.tsw_blocks ;
        begin match sw.tsw_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
            
      fprintf ppf
       "@[<1>(%s %a@ @[<v 0>%a@])@]"
       (match sw.tsw_failaction with None -> "switch*" | _ -> "switch")
       lam larg switch sw
  | TypLstaticraise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | TypLstaticcatch(lbody, (i, vars), lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        lam lbody i
        (fun ppf vars -> match vars with
          | [] -> ()
          | _ ->
              List.iter
                (fun x -> fprintf ppf " %a" typedident_print x)
                vars)
        vars
        lam lhandler
  | TypLtrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody typedident_print param lam lhandler
  | TypLifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | TypLsequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | TypLwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | TypLfor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       typedident_print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | TypLassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" typedident_print id lam expr
  | TypLsend (met, obj, largs) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(send@ %a@ %a%a)@]" lam obj lam met args largs
  | TypLevent(expr, ev) ->
      let kind = 
       match ev.lev_kind with
       | Lev_before -> "before"
       | Lev_after _  -> "after"
       | Lev_function -> "funct-body" in
      fprintf ppf "@[<2>(%s %i@ %a)@]" kind ev.lev_loc lam expr
  | TypLifused(id, expr) ->
      fprintf ppf "@[<2>(ifused@ %a@ %a)@]" typedident_print id lam expr

and sequence ppf = function
  | {tlterm=TypLsequence(l1, l2)} ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l ->
      lam ppf l

let typedlambda = lam
