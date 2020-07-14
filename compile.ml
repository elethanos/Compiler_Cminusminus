(* Top level interpreteur : utop *)
open Cparse
open Genlab
open Verbose

(*
type carte = As | Roi | Reine | Valet | Valeur of int

let carte_4 = Valeur 4

let dire_si_as carte =
  match carte with
  | As -> Printf.printf "Tu as un as !"
  | Valeur myvalue -> (Printf.printf "valeur: %d" myvalue)
  | _ -> ()


int f(int a, int f(int b[
Printf.fprintf out "int %s%s" name_var (if r == [] then "" else ",")
         *)

exception Compilation_error of string

(* let decl_list = [CDECL (_, "mavariable"); CDECL (_, "x")] *)
(* But: reproduire du c.*)

let rec compile_expr out loc_expr =
  match loc_expr with
  | (_,(VAR name_var)) -> Printf.fprintf out "%s" name_var
  | (_,(CST nombre)) -> Printf.fprintf out "%d" nombre
  | (_,(STRING chaine)) -> Printf.fprintf out " \"%s\"" chaine
  | (_,(SET_VAR (name_var, loc_expr))) -> (
    Printf.fprintf out "%s = " name_var;
    compile_expr out loc_expr;
    Printf.fprintf out ";"
  )
  | (_,(SET_ARRAY (name_var, entre_crochets, loc_expr))) -> (
    Printf.fprintf out "%s[" name_var;
    compile_expr out entre_crochets;
    Printf.fprintf out "] = ";
    compile_expr out loc_expr;
    Printf.fprintf out ";"
  )
  | (_,(CALL (name_fun, loc_expr_list))) -> (
    Printf.fprintf out "%s(" name_fun;
    List.iter (compile_expr out) loc_expr_list;
    Printf.fprintf out ");"
  )
  | (_,(OP1 (oper, loc_expr))) -> (
    match oper with
    | M_MINUS -> (Printf.fprintf out "-"; compile_expr out loc_expr)
    | M_NOT -> (Printf.fprintf out "!"; compile_expr out loc_expr)
    | M_POST_INC -> (compile_expr out loc_expr; Printf.fprintf out "++")
    | M_POST_DEC -> (compile_expr out loc_expr; Printf.fprintf out "--")
    | M_PRE_INC -> (Printf.fprintf out "++"; compile_expr out loc_expr)
    | M_PRE_DEC -> (Printf.fprintf out "--"; compile_expr out loc_expr)
  )
  | (_,(OP2 (bin_op, loc_expr1, loc_expr2))) -> (
    Printf.fprintf out "(";
    compile_expr out loc_expr1;
    let mystr = "%" in
    (match bin_op with
    | S_MUL -> (Printf.fprintf out "*"; compile_expr out loc_expr2)
    | S_DIV -> (Printf.fprintf out "/"; compile_expr out loc_expr2)
    | S_MOD -> (Printf.fprintf out "%s" mystr; compile_expr out loc_expr2)
    | S_ADD -> (Printf.fprintf out "+"; compile_expr out loc_expr2)
    | S_SUB -> (Printf.fprintf out "-"; compile_expr out loc_expr2)
    | S_INDEX -> (Printf.fprintf out "["; compile_expr out loc_expr2; Printf.fprintf out "]"));
    Printf.fprintf out ")"
  )
  | (_,(CMP (cmp_op, loc_expr1, loc_expr2))) -> (
    compile_expr out loc_expr1;
    (match cmp_op with
     | C_LT -> (Printf.fprintf out "<")
     | C_LE -> (Printf.fprintf out "<=")
     | C_EQ -> (Printf.fprintf out "=="));
    compile_expr out loc_expr2
  )
  | (_,(EIF (loc_expr1, loc_expr2, loc_expr3))) -> (Printf.fprintf out "Pas compris")
  | (_,(ESEQ loc_expr_list)) -> (Printf.fprintf out "Pas compris")

let rec compile_in_fun ?sep:(sep=",") ?final_sep:(final_sep="") out decl_list =
  match decl_list with
  | [] -> ()
  | (CDECL (_,name_var))::r -> (
    Printf.fprintf out "int %s%s" name_var (if r == [] then final_sep else sep);
    compile_in_fun out r
  )
  | _ -> raise (Compilation_error "A function can not be used as a parameter.")


let rec code_in_fun out loc_code =
  match loc_code with
  (* Version 1:
     | (_,(CBLOCK (liste_decl, mon_code::rest_code)) -> (
     compile out liste_decl;
     code_in_fun out mon_code;
     code_in_fun out (CBLOCK ([], rest_code));
     
)*)
    (* Version 2:
  | (_,(CBLOCK (liste_decl, l)) -> (
       let rec reach_list liste =
         match liste with
         | [] -> []
         | x::r -> (
           code_in_fun out x;
           reach_list r) in
       reach_list l) *)
  | (_,(CBLOCK (liste_decl, l))) -> (
    compile_in_fun out liste_decl (*~sep:";" ~final_sep:";"*);
    List.iter (code_in_fun out) l
  )

  | (_,(CEXPR loc_expr)) -> compile_expr out loc_expr
  | (_,(CIF (loc_condition, loc_code_if, loc_code_else))) -> (
    Printf.fprintf out "if(";
    compile_expr out loc_condition;
    Printf.fprintf out "){";
    code_in_fun out loc_code_if;
    Printf.fprintf out "} \n else{";
    code_in_fun out loc_code_else;
    Printf.fprintf out "}"
  )
  | (_,(CWHILE (loc_condition, loc_code))) -> (
    Printf.fprintf out "while(";
    compile_expr out loc_condition;
    Printf.fprintf out "){";
    code_in_fun out loc_code;
    Printf.fprintf out "}"
  )
  | (_,(CRETURN loc_expr_option)) -> (
    Printf.fprintf out "return ";
    (match loc_expr_option with
    | None -> Printf.fprintf out "0"
    | Some loc_expr -> compile_expr out loc_expr);
    Printf.fprintf out ";"
  )
  

(* let code_in_fun out decl_list = *)  

(* try
  raise (Compilation_error "Variable non déclarée")
with Compilation_error string_error -> Printf.printf "Il y a une erreur : %s" string_error *)



let rec compile out decl_list =
  (* genlab "mafct"; *)
  match decl_list with
  | [] -> ()
  | x::r -> (
            ( match x with
             | CDECL (_, name_var) -> Printf.fprintf out "int %s;" name_var
             | CFUN (_, name_fun, list_decla, loc_code) -> (
               Printf.fprintf out "int %s(" name_fun;
               compile_in_fun out list_decla;
               Printf.fprintf out "){";
               code_in_fun out loc_code;
               Printf.fprintf out "}"
            ));
            compile out r
  )
