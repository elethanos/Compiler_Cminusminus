open Cparse
open Genlab
open Verbose

(** Helper function to escape strings, useful when we want to put in the .asm
an actual \n coming from a printf("Hello\n") for example. **)
(* This function is usefull to be able to put accent.
   I cannot use String.escaped because it escape in
   decimal while I need here octal.
*)
let octal_escaped s =
  let count = ref 0 in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
    | '\n' | '"' | '\\' ->
      count := !count + 1
    | c when c < ' ' || c > '\x7F' ->
      count := !count + 3
    | _ -> ()
  done;
  if !count = 0 then s
  else
    let s' = Bytes.create (len + !count) in
    let j = ref 0 in
    for i = 0 to len - 1 do
      match s.[i] with
      | '"' | '\\' as c ->
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) c;
        j := !j + 2
      | '\n' ->
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) 'n';
        j := !j + 2
      | c when c < ' ' || c > '\x7F' ->
        let c = Char.code c in
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) Char.(unsafe_chr (code '0' + (c / 64) land 0x7));
        Bytes.set s' (!j + 2) Char.(unsafe_chr (code '0' + ( c / 8) land 0x7));
        Bytes.set s' (!j + 3) Char.(unsafe_chr (code '0' + (     c) land 0x7));
        j := !j + 4
      | c ->
        Bytes.set s' !j c;
        incr j
    done;
    Bytes.to_string s'


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
  | String label -> (Printf.sprintf "%s" label)
  | Register reg -> (Printf.sprintf "%s" reg)
  | Stdlib label -> (raise (Compilation_error "Stdlib should not be defined."))

let rec compile_expr (rho : env) (name_fun:string) (delta : int) loc_expr : address * code_asm * int =
  match loc_expr with
  | (_, VAR name_var) -> (Str_map.find name_var rho, "", delta)
  | (_, CST valeur) -> (Local_bp (delta + 1), Printf.sprintf "push %d\n" valeur, delta+1)
  | (_, STRING str) -> (let name = genlab name_fun in
                        (* Attention aux chaines qui contiennent des ' *)
                        (String name,Printf.sprintf "section .data\n%s: db `%s`,0 \nsection .text\n"
                                       name
                                       (octal_escaped str),
                         delta))
  | (_,SET_VAR (name_var, loc_expr)) -> 
    let (addr2, code2, delta2) = compile_expr rho name_fun delta loc_expr in
    (Str_map.find name_var rho,
     Printf.sprintf "; 123456 \n%s\n ;set var\n mov rbx, %s \n mov %s, rbx\n"
        code2
        (write_address addr2)
        (write_address (Str_map.find name_var rho)),
     delta2
    )
  | (_, SET_ARRAY (name_array, case, valeur)) -> (
    let (addr, code, delta1) = compile_expr rho name_fun delta case in
    (let (addr2, code2, delta2) = compile_expr rho name_fun delta1 valeur in
     (Local_bp (delta2 + 1),
      Printf.sprintf "%s \n%s \nlea rbx, %s \nadd rbx, %s  \nmov [rbx], %s \npush [rbx] \n"
        code
        code2
        (write_address (Str_map.find name_array rho))
        (write_address addr)
        (write_address addr2),
      delta2 + 1
     )
    )
  )
  | (_, CALL (name_fun, argu_list)) -> (
    
    let (code1, delta1, case) = List.fold_left
                                  (fun (codee, deltaa, case1) expr ->
                                    let (addr2, code2, delta2) = compile_expr rho name_fun deltaa expr in                                          
                                    (codee^code2^(Printf.sprintf "mov %s, %s \n"
                                                    (if case1 = 0 then "rdi"
                                                     else if case1 = 1 then "rsi"
                                                     else if case1 = 2 then "rdx"
                                                     else if case1 = 3 then "rcx"
                                                     else if case1 = 4 then "r8"
                                                     else if case1 = 5 then "r9"
                                                     else raise (Compilation_error "You have too many arguments"))
                                                    (write_address addr2)),
                                            delta2,
                                            case1 + 1)
                                         )
                                   ("", delta, 0)
                                   argu_list in
    (Local_bp (delta1 + 1),
    Printf.sprintf "%s \npush rsp \npush qword [rsp+0] \nand rsp, -0x10 \nextern %s \ncall %s \nmov rsp, qword [rsp + 8] \npush rax \n" code1 name_fun name_fun,
    delta1 + 1)
  )
  | (_, OP1 (oper, loc_expr)) -> (
    let (addr, code, delta1) = compile_expr rho name_fun delta loc_expr in
     (match oper with
      | M_MINUS -> (Local_bp (delta1 + 1),
                    Printf.sprintf "%s \nmov rax,%s \nneg rax \npush rax \n" code (write_address addr),
                    delta1 + 1)
      | M_NOT -> (Local_bp (delta1 + 1),
                  Printf.sprintf "%s \nmov rax, %s \nnot rax \npush rax \n" code (write_address addr),
                  delta1 + 1)
      | M_POST_INC -> (Local_bp (delta1 + 1),
                       Printf.sprintf "%s \nmov rax, %s \nadd qword %s, 1 \npush rax \n" code (write_address addr) (write_address addr),
                       delta1 + 1)
      | M_POST_DEC -> (Local_bp (delta + 1),
                       Printf.sprintf "%s \nmov rax, %s \nsub qword %s, 1 \npush rax \n" code (write_address addr) (write_address addr),
                       delta1 + 1)
      | M_PRE_INC -> (addr,
                      Printf.sprintf "%s \nadd qword %s, 1 \n" code (write_address addr),
                      delta)
      | M_PRE_DEC -> (addr,
                      Printf.sprintf "%s \nsub qword %s, 1 \n" code (write_address addr),
                      delta)
     )
  )
  | (_, OP2 (oper, loc_expr1, loc_expr2)) -> (
    let (addr, code, delta1) = compile_expr rho name_fun delta loc_expr1 in
    (let (addr2, code2, delta2) = compile_expr rho name_fun delta1 loc_expr2 in
     match oper with
     | S_MUL -> (Local_bp (delta2 + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \nmul qword %s \npush rax \n" code code2 (write_address addr) (write_address addr2), 
                 delta2 + 1)
     | S_DIV -> (Local_bp (delta2 + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \ndiv %s \npush rax \n" code code2 (write_address addr) (write_address addr2),
                 delta2)
     | S_MOD -> (Local_bp (delta + 1),
                 Printf.sprintf "%s \n%s \nmov rax, %s \ndiv %s \npush rdx \n" code code2 (write_address addr) (write_address addr2),
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
     Printf.sprintf "%s \n%s \n;; CMP \nmov rax, %s \ncmp %s, rax \n%s %s \npush 0 \njmp %s \n%s: push 1 \n%s: nop \n"
       code1
       code2
       (write_address addr2)
       (write_address addr1)
       (match cmp_op with
        | C_LT -> Printf.sprintf "jl"
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
      Local_bp (delta2), 
      Printf.sprintf "%s \n;; EIF \nmov rax, 0\ncmp rax, %s \njne %s \n%s \njmp %s \n%s: %s \n%s: nop"
        code3
        (write_address addr3)
        name_true
        code1
        name_false
        name_true
        code2
        name_false,
      delta2
    )
  )
  | (loc, ESEQ loc_expr_list) -> (
    match loc_expr_list with
    | [] -> raise (Compilation_error "This sequence is empty. \n")
    | x::[] -> compile_expr rho name_fun delta x
    | x::r -> let (addr1, code1, delta1) = compile_expr rho name_fun delta x in
              let (addr2, code2, delta2) = compile_expr rho name_fun delta1 (loc, ESEQ r) in
              (addr2,
               code1^code2,
              delta2)
  )

           
let rec local_decl (rho : env) var_decl (delta : int) : (code_asm * env * int ) =
  match var_decl with
  | CDECL (_, (name_var : string)) -> (
    Printf.sprintf "push 0 \n",
    Str_map.add name_var (Local_bp (delta + 1)) rho,
    delta + 1
  )
  | CFUN (_, name_fun, argu_list, loc_code) -> (
    raise (Compilation_error "A function can not be declared in this context.")
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
    (* to make sure we don't push multiple times in loop we reset the rsp at the end of the block
       r12 will be used to save the old value of rsp and we also save old value of r12 on the stack 
     *)
    let code0 = "push r12 \nmov r12, rsp\n" in
    let (code1, rho1, delta1) = List.fold_left gerer_list_decl (code0, rho, delta + 1) local_decl_list in
    let (code2, delta2) = List.fold_left
                            (fun (code_ass, deltaa) loca_code ->
                              let (code_, deltaa1) = compile_code rho1 name_fun deltaa loca_code in
                              (code_ass^code_, deltaa1))
                            (code1, delta1)
                            loc_code_list in
    let code3 = "mov rsp, r12 \npop r12\n" in
    (code2^code3,
     0 
    )
  )
  | (_, CEXPR loc_expr) -> (
    let (addr1, code1, delta1)  = compile_expr rho name_fun delta loc_expr in
    (Printf.sprintf "%s" code1,
    delta1)
  )
  | (_, CIF (condition, code_if, code_else)) -> (
    let (addr1, code1, delta1) = compile_expr rho name_fun delta condition in
    let (code2, delta2) = compile_code rho name_fun delta1 code_if in
    let (code3, delta3) = compile_code rho name_fun delta2 code_else in
    let name_if = genlab name_fun in
    let name_else = genlab name_fun in
    (Printf.sprintf "%s\n;; CIF\nmov rax, 0\ncmp rax, %s \nje %s \n%sjmp %s \n%s: %s \n%s: nop \n"
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
     (Printf.sprintf "%s%s: cmp qword %s, 0 \nje %s \n%sjmp %s \n%s: nop \n"
        code1
        condition_lab
        (write_address addr1)
        end_lab
        code2
        condition_lab
        end_lab,
      delta2)
  )
  | (_, CRETURN loc_expr_option) -> (
    match loc_expr_option with
    | None -> (Printf.sprintf "xor rax, rax \nleave \nret \n",
               delta)
    | Some loc_expr -> (let (addr1, code1, delta1) = compile_expr rho name_fun delta loc_expr in
                        (Printf.sprintf "%s \nmov rax, %s \nleave \nret \n" code1 (write_address addr1),
                        delta1)
                        
    )
  )

let rec global_decl (rho : env) var_decl : (code_asm * env ) = 
  match var_decl with
  | CDECL (_, name_var) -> (
    let name_lab = genlab "global" in
    (Printf.sprintf "section .data \n%s: dq 0 \nsection .text" name_lab,
    Str_map.add name_var (Global name_lab) rho)
  )
  | CFUN (_, name_fun, argu_list, loc_code) -> (
    let (code1, rho1, delta, case)  = List.fold_left
                                        (fun (codee, rhoo, deltaa, case1) argu_decl ->
                                          let (code2, rho2, delta1) = local_decl rhoo argu_decl deltaa in
                                          (codee^code2^Printf.sprintf "mov %s, %s \n"                                           
                                                         (write_address (Local_bp delta1))
                                                         (if case1 = 0 then "rdi"
                                                          else if case1 = 1 then "rsi"
                                                          else if case1 = 2 then "rdx"
                                                          else if case1 = 3 then "rcx"
                                                          else if case1 = 4 then "r8"
                                                          else if case1 = 5 then "r9"
                                                          else raise (Compilation_error "You have too many arguments")),
                                           rho2,
                                           delta1,
                                           case1 + 1)
                                        )
                                        ("", rho, 0, 0)
                                        argu_list
    in
    let (code, delta) = compile_code rho name_fun delta loc_code in
    (Printf.sprintf "%s: nop \npush rbp \nmov rbp,rsp \n%s \n%s \n" name_fun code1 code, 
     rho1
    )
  )

(* f "": nombre , je voudrais avoir nombre *)

let rec compile out decl_list =
  let (code2, rho2) = List.fold_left
                        (fun (code_asm, rho) var_decl ->
                          let (code1, rho1) =  global_decl rho var_decl in
                          (code_asm^code1,
                           rho1
                          )
                        )
                        ("", Str_map.empty)
                        decl_list in
  Printf.fprintf out "global  main\nsection   .text\n%s" code2
  
