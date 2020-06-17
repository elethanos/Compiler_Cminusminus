open Cparse

let print_locator out nom fl fc ll lc = 
	(*Format.fprintf out " file=\"%s\" first-line=\"%d\" first-column=\"%d\"" nom fl fc;
	Format.fprintf out " last-line=\"%d\" last-column=\"%d\" " ll lc;*)
	()
;;

let print_mon_op1 out=function
	|M_MINUS 	->Format.fprintf out " type=\"minus\" "
	|M_NOT 		->Format.fprintf out " type=\"not\" "
	|M_POST_INC ->Format.fprintf out " type=\"post_inc\" "
	|M_POST_DEC ->Format.fprintf out " type=\"post_dec\" "
	|M_PRE_INC 	->Format.fprintf out " type=\"pre_inc\" "
	|M_PRE_DEC	->Format.fprintf out " type=\"pre_dec\" "
;;

let print_mon_op2 out =function
	|S_MUL	->Format.fprintf out " type=\"mult\" "
	|S_DIV 	->Format.fprintf out " type=\"div\" "
	|S_MOD 	->Format.fprintf out " type=\"mod\" "
	|S_ADD 	->Format.fprintf out " type=\"add\" "
	|S_SUB 	->Format.fprintf out " type=\"sub\" "
	|S_INDEX->Format.fprintf out " type=\"index\" "
;;

let print_cmp_op out=function
	|C_LT 	->Format.fprintf out " type=\"lt\" "
	|C_LE 	->Format.fprintf out " type=\"le\" "
	|C_EQ	->Format.fprintf out " type=\"eq\" "
;;
let rec print_lock_expr out locator=function
	| VAR(str) ->(
		Format.fprintf out "<var name=\"%s\" " str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out "/>";
	)
    | CST(int)->(
		Format.fprintf out "<cst value=\"%d\" " int;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out "/>";
    )
    | STRING(str)->(
		Format.fprintf out "<str value=\"%s\" " str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out "/>";
    )
    | SET_VAR(str, loc_expr)->(
		Format.fprintf out "<set_var name=\"%s\" " str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">";
		
		let (locator, expr)=loc_expr in
		print_lock_expr out locator expr;
		
		Format.fprintf out "</set_var>";
    )
    | SET_ARRAY(str, loc_expr1, loc_expr2)->(
		Format.fprintf out "<set_array name=\"%s\" " str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">";
		
		let (locator, expr)=loc_expr1 in
		print_lock_expr out locator expr;
		
		let (locator, expr)=loc_expr2 in
		print_lock_expr out locator expr;
		
		Format.fprintf out "</set_array>";
    )
    | CALL(str, loc_exprs)->( (* appel de fonction f(e1,...,en) *)
		Format.fprintf out "<call name=\"%s\" " str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">";
		
		print_lock_exprs out loc_exprs;
		
		Format.fprintf out "</call>";
    
    )
    | OP1( mon_op, (locator1, expr))->(
		Format.fprintf out "<op1";
		
		print_mon_op1 out mon_op;
		
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">"; 
		
		print_lock_expr out locator1 expr;
		
		Format.fprintf out "</op1>";
    )
    | OP2( mon_op, (locator1, expr1), (locator2, expr2))->(
		Format.fprintf out "<op2";

		print_mon_op2 out mon_op;
		
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;

		Format.fprintf out ">"; 
		
		print_lock_expr out locator1 expr1;
		print_lock_expr out locator2 expr2;
		
		Format.fprintf out "</op2>";
	)
    | CMP(cmp_op, (locator1, expr1), (locator2, expr2))->( 
		Format.fprintf out "<cmp";
		
		print_cmp_op out cmp_op;
		
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">"; 
		
		print_lock_expr out locator1 expr1;
		print_lock_expr out locator2 expr2;
		
		Format.fprintf out "</cmp>";
		
    )
    | EIF( (locator1, expr1), (locator2, expr2), (locator3, expr3) )->( 
		Format.fprintf out "<eif>";
		print_lock_expr out locator1 expr1;
		print_lock_expr out locator2 expr2;
		print_lock_expr out locator3 expr3;
		Format.fprintf out "</eif>";
    )
    | ESEQ( loc_exprs)->(
		Format.fprintf out "<seq>";
		print_lock_exprs out loc_exprs;
		Format.fprintf out "</seq>";
    )
and print_lock_exprs out=function
	|[]->()
	|h::t->(
		let (locator, expr) = h in
		print_lock_expr out locator expr;
	)
;;


let rec print_lock_code out locator=function
	|CBLOCK(v_d_l, loc_code_list)->(
	
		Format.fprintf out "<cblocks ";
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">";
		
		print_declarations out v_d_l;
		
		print_lock_codes out loc_code_list;
		Format.fprintf out "</cblocks>";
		
	)
	|CEXPR( (locator, expr) )->(
		print_lock_expr out locator expr;
	)
	|CIF((locator1, expr1), (locator2, code2), (locator3, code3))->(
		Format.fprintf out "<if>";
		print_lock_expr out locator1 expr1;
		print_lock_code out locator2 code2;
		print_lock_code out locator3 code3;
		Format.fprintf out "</if>";
	)
	|CWHILE( (locator1, expr1), (locator2, code1))->(
		Format.fprintf out "<while>";
		print_lock_expr out locator1 expr1;
		print_lock_code out locator2 code1;
		Format.fprintf out "</while>";
	)
	|CRETURN( opt)->(
		match opt with
		|None ->()
		|Some( (locator1, expr1) )->(
			Format.fprintf out "<return>";
			print_lock_expr out locator1 expr1;
			Format.fprintf out "</return>";
		)
	)
and print_lock_codes out=function
	|[]->()
	|(locator, code)::t->(
		print_lock_code out locator code;
		print_lock_codes out t;
	)
and print_declaration out=function
	|CDECL( locator, str)->(
		Format.fprintf out "<cdecl name=\"%s\"" str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out "/>";
	)
	|CFUN( locator, str, v_d_l, l_code)->(
		Format.fprintf out "<cfun name=\"%s\"" str;
		let (nom, fl, fc, ll, lc) = locator in
		print_locator out nom fl fc ll lc;
		Format.fprintf out ">";
		
		Format.fprintf out "<args>";		
		print_declarations out v_d_l; (*args*)
		Format.fprintf out "</args>";
		
		let (locator, code) = l_code in
		print_lock_code out locator code;
		
		Format.fprintf out "</cfun>";
	)
and print_declarations out=function
	|[]-> ()
	|h::t->(
		print_declaration out h;
		print_declarations out t;
	)
;;

let print_ast out dec_list =
	Format.fprintf out "<cprog>";
	print_declarations out dec_list;
	Format.fprintf out "</cprog>";

;;
