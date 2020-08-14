open Cparse
open Genlab
open Verbose

(* Associe un nom de variable à une addresse *)
module Str_map = Map.Make(String)
type label = string

exception Compilation_error of string

(*
  Local_bp int: int >= 0, par groupe de 8 octets
*)
type address = Global of label | Local_bp of int | Stdlib of label | Register of label | String of label

(* env: nom de variable -> address *)
type env = address Str_map.t
type code_asm = string
let sp = Printf.sprintf

let write_address (my_address:address) : code_asm =
  match my_address with
  | Global label -> (Printf.sprintf "[%s]" label)
  | Local_bp num -> (Printf.sprintf "[rbp-%d]" (num*8))
  | String label -> (Printf.sprintf "[%s]" label)
  | Register reg -> (Printf.sprintf "%s" reg)
  | Stdlib label -> ()

let rec compile_expr (rho : env) (name_fun:string) (delta : int) loc_expr : address * code_asm * int =
  match loc_expr with
  | (_, VAR name_var) -> (Str_map.find name_var rho, "", delta)
  | (_, CST valeur) -> (Local_bp delta, Printf.sprintf "push %d\n" valeur, delta+1)
  | (_, STRING str) -> (let name = genlab name_fun in
                        (* Attention aux chaines qui contiennent des ' *)
                        (String name,Printf.sprintf "section .data\n%s: db '%s',0"  name str, delta))
  | (_,SET_VAR (name_var, loc_expr)) -> 
    let (addr2, code2, delta2) = compile_expr rho name_fun delta loc_expr in
    (Str_map.find name_var rho,
     Printf.sprintf "%s\n mov %s, %s\n"
        code2
        (write_address (Str_map.find name_var rho))
        (write_address addr2),
     delta2
    )
  | (_, SET_ARRAY (name_array, case, valeur)) -> (
    let (addr, code, delta1) = compile_expr rho name_fun delta case in
    (let (addr2, code2, delta2) = compile_expr rho name_fun delta1 valeur in
     (Local_bp (delta2 + 1),
      Printf.sprintf "%s \n%s \nlea rbx, %s \nadd rbx, %s  \nmov [rbx], %s \npush [rbx]"
        code
        code2
        (write_address (Str_map name_array rho))
        (write_address addr)
        (write_address addr2),
      delta2 + 1
     )
    )
  )
  | (_, CALL (name_fun, argu_list)) -> (
    
    let (addr1, code1, delta1, case) = List.fold_left
                                         (fun (addrr, codee, deltaa, case1) expr ->
                                           let (addr2, code2, delta2) = compile_expr rho name_fun deltaa expr in
                                           addr2,
                                           (codee^code2^(Printf.sprintf "mov %s, %s"
                                                           if case1 = 0 then "rsi"
                                                           else if case1 = 1 then "rdx"
                                                           else if case1 = 2 then "rcx"
                                                           else if case1 = 3 then "r8"
                                                           else if case1 = 4 then "r9"
                                                           else raise Compilation_error "You have too many arguments"
                                                                  (write_address addr2)
                                                        )
                                           ),
                                           delta2,
                                           case1 + 1
                                         )
                                   (_, "", delta, 0)
                                   argu_list in
    _,
    Printf.sprintf "push rbp \nadd rsp, 1 \nmov rbp, rsp \n%s \ncall %s \n" code1 name_fun,
    delta1
  )
  | (_, OP1 (oper, loc_expr)) -> (
    let (addr, code, delta1) = compile_expr rho name_fun delta loc_expr in
     (match oper with
      | M_MINUS -> (Local_bp (delta1 + 1),
                    Printf.sprintf "%s \nmov rax,%s \nneg rax \npush rax" code (write_address addr),
                    delta1 + 1)
      | M_NOT -> (Local_bp (delta1 + 1),
                  Printf.sprintf "%s \nmov rax, %s \nnot rax \npush rax \n" code (write_address addr),
                  delta1 + 1)
      | M_POST_INC -> (Local_bp (delta1 + 1),
                       Printf.sprintf "%s \nmov rax, %s \nadd %s, 1 \npush rax \n" code (write_address addr) (write_address adrr),
                       delta1 + 1)
      | M_POST_DEC -> (Local_bp (delta + 1),
                       Printf.sprintf "%s \nmov rax, %s \nsub %s, 1 \npush rax \n" code (write_address addr) (write_address addr),
                       delta1 + 1)
      | M_PRE_INC -> (addr,
                      Printf.sprintf "%s \nadd %s, 1 \n" code (write_address addr),
                      delta)
      | M_PRE_DEC -> (addr,
                      Printf.sprintf "%s \nsub %s, 1 \n" code (write_address addr),
                      delta)
     )
  )
  | (_, OP2 (oper, loc_expr1, loc_expr2)) -> (
    let (addr, code, delta1) = compile_expr rho name_fun delta loc_expr1 in
    (let (addr2, code2, delta2) = compile_expr rho name_fun delta1 loc_expr2 in
     match oper with
     | S_MUL -> (Local_bp (delta2 + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \nmul %s \npush rax \n" code code2 (write_address addr) (write_address addr2), 
                 delta2 + 1)
     | S_DIV -> (Local_bp (delta2 + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \ndiv %s \npush rax \n" code code2 (write_address addr) (write_address addr2),
                 delta2)
     | S_MOD -> (Local_bp (delta + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \ndiv %s \npush rdx" code code2 addr addr2,
                 delta2 + 1)
     | S_ADD -> (Local_bp (delta2 + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \nadd rax, %s \npush rax \n" code code2 (write_address addr) (write_address addr2),
                 delta2 + 1)
     | S_SUB -> (Local_bp (delta2 + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \nsub rax, %s \npush rax \n" code code2 (write_address addr) (write_address addr2),
                 delta2 + 1)
     | S_INDEX -> ( Local_bp (delta2 + 1), (* la valeur contenue dans rbx est l'adresse de la case *)
                   Printf.sprintf "%s \n%s \nlea rbx, %s \nadd rbx, %s \npush rbx \n " code code2 (write_address addr) (write_address addr2),
                   delta2 + 1)
    )
  )
  | (_, CMP (cmp_op, loc_expr1, loc_expr2)) -> (
    let (addr2, code2, delta2) = compile_expr rho name_fun delta loc_expr2 in
    let (addr1, code1, delta1) = compile_expr rho name_fun delta2 loc_expr1 in
    let name = genlab name_fun in
    let name2 = genlab name_fun in
    (Local_bp (delta1 + 1),
     Printf.sprintf "%s \n%s \ncmp %s, %s \n%s %s \npush 0 \njmp %s \n%s: push 1 \n%s: nop \n"
       code
       code2
       (write_address addr1)
       (write_address addr2)
       (match cmp_op with
        | C_LT -> Printf.sprintf "jlt"
        | C_EQ -> Printf.sprintf "je"
        | C_LE -> Printf.sprintf "jle")
       name
       name2
       name
       name2,              
     delta1 + 1)
  )
  | (_, EIF (condition, if_true, if_false)) -> (
    let (addr3, code3, delta3) = compile_expr rho name_fun delta condition in
    let (addr1, code1, delta1) = compile_expr rho name_fun delta3 if_false in
    let (addr2, code2, delta2) = compile_expr rho name_fun delta1 if_true in
    let name_true = genlab name_fun in
    let name_false = genlab name_fun in
    (
      _, 
      Printf.sprintf "%s \ncmp 1, %s \nje %s \n%s \njmp %s \n%s: %s \n%s: nop"
        code3
        (write_address addr3)
        name_true
        code1
        name_false
        name_true
        code2
        name_false,
      delta2 + 1
    )
  )
  | (_, ESEQ loc_expr_list) -> (
  )

           
let rec local_decl (rho : env) var_decl (delta : int) : (code_asm * env * int ) =
  match var_decl with
  | CDECL (_, name_var) -> (
    Printf.sprintf "push 0 \n",
    Str_map.add name_var (Local (delta + 1)),
    delta + 1
  )
  | CFUN (_, name_fun, argu_list, loc_code) -> (
    raise Compilation_error "A function can not be declared in this context."
  )

let gerer_list_decl (chaine1, rho, delta) var_decl : ( code_asm * env * int )=
  let (code1, rho1, delta1) = local_decl rho var_decl delta in
  (chaine1^code1,
   rho1,
   delta1
  )
  

let rec compile_code (rho : env) name_fun (delta : int) loc_code : (code_asm * int) = 
  match loc_code with
  | (_, CBLOCK (local_decl_list, loc_code_list)) -> (
    let (code1, rho1, delta1) = List.fold_left gerer_list_decl ("", rho, delta) local_decl_list in
    let (code2, delta2) = List.fold_left
                            (fun (code_ass, deltaa) loca_code ->
                              let (code_, deltaa1) = compile_code rho1 name_fun deltaa loca_code in
                              (code_ass^code_, deltaa1))
                            (code1, delta1)
                            loc_code_list in
    (code2,
     delta2 
    )
  )
  | (_, CEXPR loc_expr) -> (
    let (addr1, code1, delta1)  = compile_expr name_fun delta loc_expr in
    (Printf.sprintf "%s" code1,
    delta1)
  )
  | (_, CIF (condition, code_if, code_else)) -> (
    let (addr1, code1, delta1) = compile_expr rho name_fun delta condition in
    let (code2, delta2) = compile_code rho name_fun delta1 code_if in
    let (code3, delta3) = compile_code rho name_fun delta2 code_else in
    let name_if = genlab name_fun in
    let name_else = genlab name_fun in
    (Printf.sprintf "%scmp 0, %s \nje %s \n%sjmp %s \n%s: %s \n%s: nop \n"
      code1
      (write_address addr1)
      name_else
      code2
      name_if
      name_else
      code3
      name_if,
     delta3)
  )
  | (_, CWHILE (condition, code)) -> (
    let (addr1, code1, delta1) = compile_expr rho name_fun delta condition in
     let (code2, delta2) = compile_code rho name_fun delta1 code in
     let condition_lab = genlab name_fun in
     let end_lab = genlab name_fun in
     (Printf.sprintf "%s%s: cmp 0, %s \nje %s \n%sjmp %s \n%s: nop \n"
        code1
        condition_lab
        (write_address addr1)
        end_lab
        code2
        condition_lab
        end_lab,
      delta2)
  )

let rec global_decl (rho : env) var_decl : (code_asm * env ) = 
  match var_decl with
  | CDECL (_, name_var) -> (
    let name_lab = genlab "global" in
    (Printf.sprintf "section .data \n%s: dq 0 \nsection .text" name_lab,
    Str_map.add name_var (Global name_lab) rho)
  )
  | CFUN (_, name_fun, argu_list, loc_code) -> (
    let name_lab = genlab name_fun in
    let (code1, rho1)  = List.fold_left
                 (fun (rhoo, codee) argu_decl ->
                   let (code2, rho2) = global_decl rhoo argu_decl in
                   (codee^code2,
                    rho2 
                   )
                     ("", rho)
                     argu_list
                 ) in
    let (code, delta) = compile_code rho name_fun 0 loc_code in
    (Printf.sprintf "%s: nop \n%s \n%s" name_lab code1 code, (* gerer la declaration des arguments*)
     rho1
    )
  )
