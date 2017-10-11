(* 
	Vérifier qu'il y a un retourne
*)
(* Arbres de syntaxe abstraite *)

type ('info, 'node) node = { info : 'info;
			     node : 'node }

type loc = Lexing.position * Lexing.position

type ident = (loc, string) node

type signedness = Unsigned | Signed
type num = Char | Short | Int | Long

type unop  =
  | Ref
  | Uminus
  | Uplus
  | Not
  | Pre_dec
  | Pre_inc
  | Post_dec
  |	Post_inc
  | Addr

type binop =
  | Plus | Minus | Mult | Div
  | And  | Or
  | Eq   | Neq   | Lt   | Le  | Gt | Ge
  | Mod
  
type c_type =
  | Tnull (* pour typer null *)
  | Tvoid
  | Tint of signedness * num
  | Tdouble
  | Tstruct of ident
  | Tpoint of c_type

type constant =
  | Cint of signedness * num * string
  | Cdouble of float
  | Cstring of string


type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
  | Econst of constant
  | Eident of ident
  | Esizeof of c_type
  | Epoint of 'info expr
  | Egetarr of 'info expr * 'info expr
  | Egetstr of 'info expr * ident
  | Eassign of 'info expr * 'info expr
  | Ecall of ident * 'info expr list
  | Ebinop of binop * 'info expr * 'info expr
  | Eunop of unop * 'info expr
  | Ecast of c_type * 'info expr


type var_decl =  c_type * ident

type 'info statement = ('info, 'info statement_node) node

and 'info statement_node =
  | Sskip
  | Sbloc of 'info block
  | Sexpr of 'info expr
  | Sreturn of 'info expr option
  | Sif of 'info expr * 'info statement * 'info statement option
  | Sfor of 'info expr list * 'info expr option * 'info expr list * 'info statement

and 'info block =
    var_decl list * 'info statement list

type 'info decl =
  | Dvar of var_decl
  | Dtype of ident * var_decl list
  | Dfun of c_type * ident * var_decl list * 'info block option

type 'info file =  'info decl list
