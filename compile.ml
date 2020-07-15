open Cparse
open Genlab
open Verbose


let rec arg_decl out decl_list =
  match decl_list with
  | [] -> ()
  | (CDECL (_,name_var))::r -> (
   (* declarer les arguments pris par une fonction lors de la declaration "int main(int nb, int k)" *) 
  )

let rec compile_code out loc_code =
  match loc_code with
  | (_, CBLOCK (var_decl_list, loc_code_list)) -> (
    var_decl out var_decl_list;
    List.iter (compile_code out) loc_code_list
  )
  | (_, CEXPR loc_expr) -> compile_expr out loc_expr
  | (_, CIF (condition, loc_code1, loc_code2)) -> (
    compile_expr out condition;
    Printf.fprintf out "if_true: "; (*on donne un label pour pouvoir s'y referer lors du saut conditionnel*)
    compile_code out loc_code1;
    Printf.fprintf out "\n";
    Printf.fprintf out "else: ";
    compile_code out loc_code2;
    Printf.fprintf out "\n"
  )
  | (_, CWHILE (condition, loc_code)) -> (
    Printf.fprintf out "start_loop";
    compile_code out loc_code;
    Printf.fprintf out "\n"
    compile_expr out condition;
    Printf.fprintf out "start_loop \n" (* le debut sera complete lors de la comparaison *)
  )
  | (_, CRETURN loc_expr_option) -> (
    match loc_expr_option with
    | None -> ()
    | Some loc_expr ->() (* quel registre utilise-t-on pour faire un return *)
  )
        
let rec var_decl out decl_list =
  match decl_list with
  | [] -> ()
  | x::r -> (
    (match x with
     | CDECL (_, name_var) -> Printf.fprintf out "%s: mov bx, 0 \n" name_var (* faut-il utiliser un registre ? *)
     | CFUN (_, name_fun, arg_list, loc_code) -> (
       Printf.fprintf out "%s: " name_fun;
       arg_decl out arg_list;
       compile_code out loc_code
    ));
    var_decl out r
  )

           
