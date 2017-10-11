(* Analyse lexicale *)
{
  open Lexing
  open Parser
  open Ast

  (* Erreurs lexicales *)
  exception Lexical_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum;
        pos_cnum=0 }
		
  (* Retourne l'identifiant *)
  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ 
	"char", 	CHAR;
	"double",	DOUBLE;
	"long",		LONG;
	"int",		INT;
	"short",	SHORT;
	"unsigned",	UNSIGNED;
	"if",    	IF;
	"else",  	ELSE;
	"extern", 	EXTERN;
	"return",  	RETURN;
	"struct",  	STRUCT;
	"void",  	VOID;
	"while", 	WHILE;
	"for",   	FOR;
	"sizeof",   SIZEOF;
      ]	;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT s
  (* Permet de décoder un caractère : Pour les String et Char *)
  let decode_char c i =
	let i = ref i in
	let c =
		match c.[!i] with
		| '\\' -> 
			begin
				i:=!i+1;
				match c.[!i] with
				| 'x' ->
					let dec_hex x =
							match x with
							|'0' -> 0
							|'1' -> 1
							|'2' -> 2
							|'3' -> 3
							|'4' -> 4
							|'5' -> 5
							|'6' -> 6
							|'7' -> 7
							|'8' -> 8
							|'9' -> 9
							|'A'|'a' -> 10
							|'B'|'b' -> 11
							|'C'|'c' -> 12
							|'D'|'d' -> 13
							|'E'|'e' -> 14
							|'F'|'f' -> 15
							| _ -> assert false
					in i:=!i+2; (16*(dec_hex c.[!i-1])+(dec_hex c.[!i]))
				| '\\' -> 92
				| '\'' -> 39
				| '\"' -> 34
				| 'n' -> 10
				| 't' -> 9
				| _ -> assert false
			end
		| _ -> 
			Char.code c.[!i]
	in (c, !i+1)
}

let chiffre = ['0'-'9']
let integer = chiffre+
let double = (integer['.']chiffre* | ['.']integer)(['e''E']['-']?integer)?
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | ['_']) (chiffre | alpha | ['_'])*
let simple_char = [' ' - '~']#['\'' '\"' '\\']
let hexa = (chiffre|['a'-'f' 'A'-'F'])
let char = simple_char | '\\'('\\'|'\''|'\"'|'n'|'t'|('x' hexa hexa))

rule token = parse
	(* Gestion du retour à la ligne *)
  | '\n'
      { newline lexbuf; token lexbuf }
	(* Gestion du caractère inutile *)
  | [' ' '\t' '\r']+
      { token lexbuf }
	(* Gestion des identifiants *)
  | ident
	  { keyword_or_ident (lexeme lexbuf) }
	(* Gestion des Int *)
  | (integer as i)(['U''u']? as u)(['L''l']? as l)
	{ 
		let s = if u = "" 
			then Ast.Signed
			else Ast.Unsigned 
		in 
		let t = if l =""
			then Ast.Int 
			else Ast.Long
		in
		try CONST_INT(s, t, i)
			with _ -> raise (Lexical_error ("illegal integer constant: " ^i))
	}
	(* Gestion des Double *)
  | double as d
	{ CONST_DOUBLE (float_of_string (d))}
	(* Gestion des String *)
  | ('\"')(char* as s)('\"') {
		let explode s =
		  let rec expl max i l =
			if i > max then l else
				let (c,i) = decode_char s i in
					expl max (i) (l@[(Char.chr c)])
			in
			expl (String.length s - 1) 0 []
		in let cl = explode s in
		let implode l =
			let result = Bytes.create (List.length l) in
				let rec imp i = function
				| [] -> result
				| c :: l -> Bytes.set result i c; imp (i + 1) l in
				imp 0 l
		in let s = implode cl in
		CONST_STRING s
	}
	(* Gestion des Char *)
  | ('\'')(char as c)('\'') {
		let (c,x) =
			decode_char c 0
		in 
			try CONST_INT(Ast.Signed, Ast.Char, string_of_int c)
				with _ -> raise (Lexical_error ("illegal char"))
	}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "-" { MINUS }
  | "+" { PLUS }
  | "*" { MULT }
  | "/" { DIV }
  | "%" { MOD }
  | "==" { EQ }
  | "!=" { NEQ }
  | ">" { GT }
  | ">=" { GE }
  | "<" { LT }
  | "<=" { LE }
  | "&&" { AND }
  | "||" { OR }
  | "=" { ASSIGN }
  | ";" { SEMI }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | "." { DOT }
  | "->" { ARROW }
  | "--" { DEC }
  | "++" { INC }
  | "!" { NOT }
  | "&" { ADDR }
  | "//" { comment_line lexbuf; token lexbuf }
  | "/*" { comment_bloc lexbuf; token lexbuf }
  | eof { EOF }
  |	"#include <stdlib.h>" | "#include <stdio.h>" { token lexbuf}
  | _ { raise (Lexical_error ("Illegal character: " ^ lexeme lexbuf)) }  
	
(* Les commentaires sur une ligne *)
and comment_line = parse
  | '\n' {newline lexbuf}
  | eof {}
  | _ {comment_line lexbuf}

(* Les commentaires en bloc *)
and comment_bloc = parse
  | "/*"
	{ raise (Lexical_error ("Illegal comment (imbricated)")) }	
  | eof
	{ raise (Lexical_error ("Unterminated comment")) }
  | "*/"
	{}
  | "\n"
    {newline lexbuf; comment_bloc lexbuf}
  | _
	{comment_bloc lexbuf}
