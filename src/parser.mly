/* Parseur pour le compilateur C */

%{
  open Ast


  let mk_loc e l = { info = l; node = e }

  let rec start_type cpt ty = 
	match cpt with
	| 0 -> ty
	| _ -> Tpoint(start_type (cpt-1) ty)
	
	(* Permet d'initialiser les Tpoint *)
  let init_var (star,id) ty = 
	(start_type star ty), id

%}

%token EOF
%token <string> CONST_STRING
%token <float> CONST_DOUBLE
%token <Ast.signedness*Ast.num*string> CONST_INT
%token CHAR DOUBLE LONG INT SHORT
%token UNSIGNED
%token IF ELSE
%token EXTERN
%token RETURN
%token WHILE FOR
%token SIZEOF
%token VOID
%token STRUCT
%token PLUS MINUS MULT DIV MOD
%token AND OR NOT
%token EQ NEQ
%token GE GT LE LT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token COMMA
%token SEMI
%token ASSIGN
%token ARROW DOT
%token DEC INC
%token ADDR
%token <string> IDENT

%nonassoc IF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ 
%left GE GT LE LT
%left PLUS MINUS
%left MULT DIV MOD
%left RPAREN LBRACKET ARROW DOT
%right NOT INC DEC ADDR 

/* Point d'entrée */

%start file
%type <Ast.loc Ast.file> file
%%

file:
| decls=list(decl); EOF { decls }
;

decl :
| d=decl_vars 		{ Dvar(d) }
| d=decl_typ 		{ let (id,v)=d in Dtype(id,v) }
| d=decl_fct 		{ let (ty, id, args, b)=d in Dfun(ty, id, args, Some b) }
| d=decl_ext 		{ let (ty, id, args)=d in Dfun(ty, id, args, None) }
;

decl_vars :
| ty=typ; v=var; SEMI	 { init_var v ty }
;

decl_typ :
| STRUCT; id=ident; LBRACE; vars=list(decl_vars); RBRACE; SEMI { (id, vars) }
;

decl_fct :
| ty=typ; v=var; LPAREN; args=separated_list(COMMA, argument); RPAREN; b=bloc	{ let (cpt,id)=v in ((start_type cpt ty), id, args, b) }
;

decl_ext :
| EXTERN; ty=typ; v=var; LPAREN; args=separated_list(COMMA, argument); RPAREN; SEMI	{ let (cpt,id)=v in ((start_type cpt ty), id, args) }
;

typ :
| VOID									{ Tvoid 		}
| ty=int_type 							{ ty 			}
| DOUBLE 								{ Tdouble 		}
| STRUCT; id=ident 						{ Tstruct(id) 	}
;

int_type :
| sign=int_sign; size=int_size			{ Tint (sign, size) }
;

argument :
| ty=typ; v=var 						{ init_var v ty }
;

var :
| id=ident								{ (0, id) 							}
| MULT; v=var							{ let (cpt,id)=v in ((cpt+1), id) 	}
;

ident : 
| id=IDENT								{ mk_loc id ($startpos, $endpos) }
;

expr_ :
| id=ident								{ (Eident(id)) 														}
| c=const								{ (Econst(c)) 														}
| e1=expr; LBRACKET; e2=expr; RBRACKET 	{ (Eunop(Ref, mk_loc (Ebinop(Plus, e1, e2)) ($startpos, $endpos))) 	}
| e=expr; DOT; i=ident					{ (Egetstr(e, i)) 													}
| e=expr; ARROW; i=ident				{ (Egetstr(mk_loc (Eunop(Ref, e)) ($startpos, $endpos), i)) 		}
| e1=expr; ASSIGN; e2=expr				{ (Eassign(e1, e2)) 												}
| i=ident; LPAREN; l=l_expr; RPAREN		{ (Ecall(i, l)) 													}
| e1=expr; b=binop; e2=expr 			{ (Ebinop(b, e1, e2))												}
| u=unop; e=expr						{ (Eunop(u, e))														}
| e=expr; INC							{ (Eunop(Post_inc, e))												}
| e=expr; DEC							{ (Eunop(Post_dec, e))												}
| SIZEOF; LPAREN; ty=cplx_type; RPAREN	{ (Esizeof(ty))														}
| LPAREN; ty=cplx_type; RPAREN; e=expr	{ (Ecast(ty, e))													}
| LPAREN; e=expr_; RPAREN				{ e 																}
;

expr :
| e=expr_								{ mk_loc e ($startpos, $endpos)			}
;
const:
| s=CONST_STRING 						{ Cstring s 							}
| d=CONST_DOUBLE 						{ Cdouble d 							}
| i=CONST_INT  							{ let (s, ty, v)=i in Cint (s, ty, v)	}
;

cplx_type :
| ty=typ; 								{ ty }
| ty=cplx_type; MULT					{ ty }
;

l_expr :
| l=separated_list(COMMA, expr)			{ l  }
;

instruction_ :
| SEMI 																					{ Sskip						}
| e=expr; SEMI																			{ (Sexpr(e))				}
| IF; LPAREN; e=expr; RPAREN; i=instruction	%prec IF									{ (Sif(e, i, None)) 		}
| IF; LPAREN; e=expr; RPAREN; i1=instruction; ELSE; i2=instruction 						{ (Sif(e, i1, Some i2)) 	}
| WHILE; LPAREN; e=expr; RPAREN; i=instruction											{ (Sfor([], Some e, [], i))	}
| FOR; LPAREN; e1=l_expr; SEMI; e2=option(expr); SEMI; e3=l_expr; RPAREN; i=instruction	{ (Sfor(e1, e2, e3, i)) 	}
| b=bloc																				{ (Sbloc b) 				}
| RETURN; e=option(expr); SEMI 															{ (Sreturn(e)) 				}
;

instruction :
| i=instruction_	{ mk_loc i ($startpos, $endpos)}
;

bloc :
| LBRACE; d=list(decl_vars); i=list(instruction); RBRACE {(d, i)}
;

%inline int_sign:
| UNSIGNED  { Unsigned	}
| 			{ Signed	}
;

%inline int_size:
| CHAR  { Char  }
| SHORT { Short }
| INT  	{ Int  	}
| LONG  { Long  }
;

%inline binop:
| PLUS  { Plus  }
| MINUS { Minus }
| MULT  { Mult  }
| DIV   { Div   }
| AND   { And   }
| OR	{ Or	}
| EQ	{ Eq	}
| NEQ   { Neq   }
| LT	{ Lt	}
| LE	{ Le	}
| GT	{ Gt	}
| GE	{ Ge	}
| MOD   { Mod   }

%inline unop:
| MULT	{ Ref	}
| MINUS { Uminus}
| PLUS 	{ Uplus	}
| NOT   { Not	}
| DEC	{ Pre_dec }
| INC	{ Pre_inc }
| ADDR	{ Addr	}
;
