open Ast

(* Initialisation des environnements *)
module Env = Map.Make(String)
let global_env = Hashtbl.create 17
let struct_env : (string, var_decl list) Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17

(* Initialisation de Tint *)
let unsigned_int = Tint(Unsigned, Int)
let signed_int = Tint(Signed, Int)
let unsigned_long = Tint(Unsigned, Long)
let signed_long = Tint(Signed, Long)

(* Exceptions *)
exception TypeError of loc * string
let error loc msg = raise (TypeError (loc, msg))

(* Permet d'afficher les types (Utilisé dans les messages de debug) *)
let print_type t =
	let rec print_type_aux t =
		match t with
		| Tstruct id -> "structure " ^ id.node
		| Tpoint tt1 -> (print_type_aux tt1) ^ "*" 
		| Tnull -> "null"
		| Tvoid -> "void"
		| Tdouble -> "double"
		| Tint(sign, num) -> 
			begin
				let num = match num with 
					| Char	-> "char"
					| Short	-> "short"
					| Int	-> "int"
					| Long	-> "long"
					in
				match sign with 
				| Unsigned -> "unsigned " ^ num
				| Signed -> num
			end
	in ("\'" ^ (print_type_aux t) ^ "\'")

(* Permet d'afficher les binop (Utilisé dans les messages de debug) *)
let print_binop b =
	let name = 
		match b with
		| Plus  -> "+"
		| Minus -> "-"
		| Mult  -> "*"
		| Div   -> "/"
		| And   -> "&&"
		| Or    -> "||"
		| Eq    -> "=="
		| Neq   -> "!="
		| Lt    -> "<"
		| Le    -> "<="
		| Gt    -> ">"
		| Ge    -> ">="
		| Mod   -> "%"
	in ("binary operator \'" ^ name ^ "\'")

(* Permet d'afficher les unop (Utilisé dans les messages de debug) *)
let print_unop u =
	let name = 
		match u with
		| Ref    -> "*"
		| Uminus -> "-"
		| Uplus  -> "+"
		| Not    -> "!"
		| Pre_dec | Post_dec -> "--"
		| Pre_inc | Post_inc -> "++"
		| Addr   -> "&"
	in ("unary operator \'" ^ name ^ "\'")
	
(* Permet de vérifier si 2 types sont compatibles *)
let compatible t1 t2 =
	let rec compat_aux t1 t2 =
		match t1, t2 with
		| Tstruct id1, Tstruct id2 -> id1.node = id2.node
		| Tpoint(Tvoid), Tpoint _ -> true
		| Tpoint tt1, Tpoint tt2 -> compat_aux tt1 tt2
		| Tnull, Tpoint _ -> true
		| ((Tdouble | Tnull | Tint(_)), (Tdouble | Tnull | Tint(_))) -> true
		| _ -> false
	in
	compat_aux t1 t2 || compat_aux t2 t1

(* Permet de vérifier si 2 types sont exactement égaux *)
let equals t1 t2 =
	let rec equals_aux t1 t2 =
		match t1, t2 with
		| Tstruct id1, Tstruct id2 -> id1.node = id2.node
		| Tpoint tt1, Tpoint tt2 -> equals_aux tt1 tt2
		| Tnull, Tnull -> true
		| Tdouble, Tdouble -> true
		| Tint(signe1, num1), Tint (signe2, num2) -> 
			signe1 = signe2 && num1 = num2
		| _ -> false
	in
	equals_aux t1 t2

(* Raccourci de créer un node *)
let mk_node t e = {info = t; node = e}

(* Raccourci de créer cast *)
let mk_cast t e =
	if (compatible t e.info) then
		mk_node t (Ecast(t,e))
	else
		assert false

(* Permet de vérifier si c'est un Tdouble *)
let is_double t = 
	match t with
	| Tdouble -> true
	| _ -> false
	
(* Permet de vérifier si c'est un Num *)
let num t =
	match t with
	| Tstruct _ | Tvoid -> false
	| _ -> true

(* Permet de vérifier si c'est un Arith *)
let arith t =
	match t with
	| Tstruct _ | Tvoid | Tpoint _ -> false
	| _ -> true	
	
(* Permet de connaitre la taille d'un Num *)
let rank t =
	let rank_aux n = 
		match n with 
		| Char	-> 7
		| Short	-> 15
		| Int	-> 31
		| Long	-> 63
	in
	match t with
	| Tint (Signed, n)	-> rank_aux n
	| Tint (Unsigned, n)-> 1 + rank_aux n
	| Tdouble			-> 100
	| Tnull				-> 0
	| _ 				-> assert false

(* Permet de savoir si le type 1 a une taille inférieure au type 2 *)
let inf_type t1 t2 =
	rank t1 < rank t2

(* Retourne le type avec la plus grande taille *)	
let max_type t1 t2 =
	if inf_type t1 t2 then t2
	else t1
	
(*  *)
let rec type_bf t : bool =
	match t with
	| Tpoint tt -> type_bf tt
	| Tstruct td -> Hashtbl.mem struct_env td.node
	| _ -> true

(* Permet de déclarer une liste variable *)
let type_var_decl vd =
	let _ = 
		List.fold_left (fun acc (t, id) ->
			if t = Tvoid || not (type_bf t) then
				error id.info ("Invalid type " ^ (print_type t) ^ " for " ^ id.node)
			else if List.mem id.node acc then
				error id.info ("Redefinition of ident " ^ id.node)
			else 
				id.node::acc) [] vd
	in vd	
	
(* Permet d'ajouter une element à un environnement *)
let check_env tab key v =
	if Hashtbl.mem tab key.node then 
		error key.info ("Redefinition of ident " ^ key.node)
	else
		Hashtbl.add tab key.node v
	
(* Permet d'ajouter une liste d'element à un environnement *)
let add_env env vd =
	List.fold_left (fun acc (t, id) -> Env.add id.node t acc) env vd

(* Typage des constantes *)
let type_const c =
	match c with
	| Cstring _				-> Tpoint (Tint(Signed, Char))
	| Cdouble _				-> Tdouble
	| Cint(Signed, Int, "0")-> Tnull
	| Cint(s, i, _)			-> Tint (s, i)

(* Typage des expressions *)	
let rec type_expr env e = 
	match e.node with
	| Econst c				->
		let t_c = type_const c
		in mk_node t_c (Econst c)
	| Esizeof t				->
		if t = Tvoid || not (type_bf t) then
			error e.info ("Can't use sizeof on type " ^ (print_type t))
		else
			mk_node (Tint(Unsigned, Long)) (Esizeof t)
	| Eassign (e1, e2)		->
		let e1 = (type_lvalue env e1) in
		let e2 = (type_expr env e2) in
		if compatible e1.info e2.info then
			mk_node e1.info (Eassign (e1, (mk_cast e1.info e2)))
		else
			error e.info ("Can't assign type " ^ (print_type e2.info) ^ " to " ^ (print_type e1.info))
	| Ebinop(binop, e1, e2)	->
		begin
			let te1 = type_expr env e1 in
			let te2 = type_expr env e2 in
			match (binop, te1.info, te2.info) with
			| (_, Tpoint _, Tpoint _) ->
				if binop == Minus then 
					if equals te1.info te2.info then
						mk_node (Tint(Signed, Long)) (Ebinop(binop,te1,te2))
					else
						error e1.info ("Can't sub types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
				else 
					error e1.info ("Can't use binary operator " ^ (print_binop binop) ^ " on pointers " ^(print_type te1.info) ^ " and " ^ (print_type te2.info))
			| (_, Tpoint _, _) ->
				begin 
					match binop with
					| Plus | Minus ->
						let m  = max_type te2.info (Tint(Unsigned, Long)) in 
						if rank m <= 64 then
							mk_node te1.info (Ebinop(binop,te1,te2))
						else
							error e2.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
					| _ -> error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
				end
			| (_, _, Tpoint _) ->
				begin 
					match binop with
					| Plus ->
						let m  = max_type te1.info (Tint(Unsigned, Long)) in 
						if rank m <= 64 then
							mk_node te2.info (Ebinop(binop,te1,te2))
						else
							error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
					| _ -> error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
				end
			| _ ->
				begin
					let t1 = te1.info in
					let t2 = te2.info in 
					let te1, te2 =
						if arith t1 && arith t2 && not (equals t1 t2) then
							if is_double t1 then te1, mk_cast Tdouble te2
							else if is_double t2 then  mk_cast Tdouble te1, te2
							else
								let te1 = if inf_type t1 signed_int then mk_cast signed_int te1 else te1 in
								let te2 = if inf_type t2 signed_int then mk_cast signed_int te2 else te2 in
								let t1 = te1.info in
								let t2 = te2.info in 
								if equals t1 unsigned_long then te1, mk_cast unsigned_long te2
								else if equals t2 unsigned_long then mk_cast unsigned_long te1, te2
								else if equals t1 signed_long then te1, mk_cast signed_long te2
								else if equals t2 signed_long then mk_cast signed_long te1, te2
								else if equals t1 unsigned_int then te1, mk_cast unsigned_int te2
								else if equals t2 unsigned_int then mk_cast unsigned_int te1, te2
								else te1, te2
						else te1, te2
					in
					match binop with
					| Plus | Minus | Mult | Div ->
						if compatible te1.info te2.info then
							if compatible te1.info Tdouble then
								mk_node (max_type te1.info te2.info) (Ebinop(binop,te1,te2))
							else 
								error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
						else
							error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
					| And | Or ->
						if compatible te1.info te2.info then
							if compatible te1.info Tdouble then
								mk_node (Tint(Unsigned, Int)) (Ebinop(binop,te1,te2))
							else 
								error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
						else
							error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
					
					| Eq | Neq | Lt | Le | Gt | Ge ->
						if num te1.info then
							if compatible te1.info te2.info then
								mk_node (Tint(Unsigned, Int)) (Ebinop(binop,te1,te2))
							else 
								error e.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
						else
							error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
					| Mod ->
						let m  = max_type te1.info te2.info in 
						if rank m <= 64 then
							mk_node m (Ebinop(binop,te1,te2))
						else
							error e1.info ("Can't use " ^ print_binop binop ^ " between types " ^ (print_type te1.info) ^ " and " ^ (print_type te2.info))
				end
		end
	| Eunop (unop , e0) ->
		begin
		match unop with
		| Uminus| Uplus ->
			let te = type_expr env e0 in
			if not(arith te.info) then
				error e.info ("Can't use " ^ print_unop unop ^ " on type " ^ (print_type te.info))
			else
				mk_node te.info (Eunop(unop, te))
		| Not ->
			let te = type_expr env e0 in
			if not(num te.info) then
				error e.info ("Can't use " ^ print_unop unop ^ " on type " ^ (print_type te.info))
			else
				mk_node (Tint(Unsigned, Int)) (Eunop(unop, te))
		| Pre_inc | Pre_dec | Post_inc | Post_dec ->
			let te = type_lvalue env e0 in
			if not(num te.info) then
				error e.info ("Can't use " ^ print_unop unop ^ " on type " ^ (print_type te.info))
			else
				mk_node te.info (Eunop(unop, te))
		| Addr ->
			let te = type_lvalue env e0 in
			mk_node (Tpoint(te.info)) (Eunop(unop, te))
		| Ref ->
			type_lvalue env e
		
		end
	| Ecast (t, e) ->
		let te = type_expr env e in
		if ((num t)&&(num te.info)&&(type_bf t)) then
			mk_node t (Ecast(t,te))
		else
			error e.info ("Can't cast " ^ print_type te.info ^ " to " ^ print_type t)
	| Ecall (f, params) -> 
		let tparams = List.map (type_expr env) params in
		begin
			try
				let tret, _, args, _ = Hashtbl.find fun_env f.node in
				try
					let new_params = 
						if args = [] then
							tparams
						else
							List.map2 (fun e(t, x) -> 
								if not (compatible e.info t) then
									error x.info ("Invalid type "^print_type e.info^" for "^x.node^" in "^f.node^", "^print_type t^" expected")
								else 
									mk_node t (Ecast(t, e))
						) tparams args in
					mk_node tret (Ecall(f, new_params))
				with
					Invalid_argument _ -> error f.info ("Invalid number of arguments for "^f.node)
			with
				Not_found -> error f.info ("Function "^f.node^" does not exists")
		end
	| Egetstr (e, i) ->
		type_eaccess type_expr env e i
	|_ -> type_lvalue env e

and type_lvalue env e =
	match e.node with
	| Eident id ->
		let t =
		try
			try
				Env.find id.node env
			with
				Not_found -> Hashtbl.find global_env id.node
		with
			Not_found -> error id.info ("Variable "^id.node^" is not defined ")
		in mk_node t (Eident id)
	| Egetstr (e, i) ->
		type_eaccess type_lvalue env e i
	| Eunop(Ref, e) ->
		begin
			let te = type_expr env e in
			match te.info with
			| Tpoint t ->
				mk_node t (Eunop(Ref,te))
			| _ ->
				error e.info "Pointer expected"
		end
	| _ -> error e.info "Left value expected"

and type_eaccess f_type env e i =
	begin
		let te = f_type env e in
		match te.info with
		| Tstruct s ->
			begin
				let vars = Hashtbl.find struct_env s.node in
				try
					let ti,_ = List.find (fun (t,y) -> y.node = i.node) vars in
					mk_node ti (Egetstr(te, i))
				with
					Not_found -> error i.info ("Variable " ^ i.node ^ " not defined in struct "^s.node)
			end
		| _ ->
			error e.info ("Structure expected")
	end
	
(* Typage des instructions *)
let rec type_instr t env instr = 
	match instr.node with
	| Sskip				-> 
		false, (mk_node Tvoid Sskip)
	| Sbloc (b) 		-> 
		let return, b = type_block t env b in 
		return, (mk_node Tvoid (Sbloc b))
	| Sexpr	(e)			-> 
		let e = type_expr env e in 
		false, (mk_node e.info (Sexpr e))
	| Sreturn None		-> 
		if compatible t Tvoid then 
			true, (mk_node Tvoid (Sreturn None))
		else 
			error instr.info ("Invalid return type " ^ (print_type t))
	| Sreturn Some e	-> 
		let e = (type_expr env e) in
		if compatible t e.info then 
			true, (mk_node e.info (Sreturn (Some e)))
		else 
			error instr.info ("Invalid return type " ^ (print_type e.info) ^ ", " ^ (print_type t) ^ " expected") 
	| Sif (e, i1, i2)	->
		let e = (type_expr env e) in
		if num e.info then begin
			let return1, i1 = (type_instr t env i1) in
			let return2, i2 = match i2 with
			| None -> false, None
			| Some i2 -> 
				let return2, i2 = type_instr t env i2 in
				return2, (Some i2)
			in
			(return1 && return2), (mk_node Tvoid (Sif(e, i1, i2)))
		end
		else 
			error instr.info ((print_type e.info) ^" is not a valid condition type")
	| Sfor (e1, e2, e3, i) ->
		let e1 = List.map (type_expr env) e1 in
		let e2 = match e2 with
			| None -> None
			| Some e2 -> let e2 = type_expr env e2 in
				if num e2.info then
					Some e2
				else
					error instr.info ((print_type e2.info) ^" is not a valid condition type")
			in
		let e3 = List.map (type_expr env) e3 in
		let return, i = (type_instr t env i) in
		(* On n'est pas obligé de rentrer dans la boucle for donc on ne peut pas être sûre de faire le retour *)
		false, (mk_node Tvoid (Sfor(e1,e2,e3,i)))
	
(* Typage des blocs *)
and type_block t env (var_decl, instrs)  = 
	let t_v = type_var_decl var_decl in
	let env = add_env env var_decl in
	
	let returns = ref false in
	let t_i = List.map (fun instr -> 
		let return, i = type_instr t env instr in
		returns := !returns || return;
		i) instrs 
	in !returns, (t_v, t_i)

(* Typage des déclarations *)
let type_decl d =
	match d with
	| Dvar(t, i) -> 
		if type_bf t && t <> Tvoid then begin
			check_env global_env i t;
			Dvar ((t, i))
		end
		else
			error i.info ("Invalid type " ^ (print_type t) ^ " for " ^ i.node)
	| Dtype (id, var_decl) ->
		check_env struct_env id var_decl;
		let t_v = type_var_decl var_decl in 
		Dtype (id, t_v)
	| Dfun (t, f, params, b) ->
		if not (type_bf t) then 
			error f.info ("Invalid return type "^print_type t^" for function " ^ f.node)
		else 
			check_env fun_env f (t, f, params, (b!=None));
			let t_v = type_var_decl params in
			let t_b = 
				match b with
				| None -> None
				| Some block -> 
					let env = add_env Env.empty params in 
					let return, t_b = type_block t env block in 
					if t == Tvoid || return then
						Some t_b
					else
						error f.info ("A return statement is missing in the function : " ^ f.node);
			in
			Dfun(t, f, t_v, t_b)
	
(* Typage du programme *)
let type_prog l = 
	let result = List.map (type_decl) l in
	try
		let tret, f, args , _ = Hashtbl.find fun_env "main" in
		begin
			if not (equals tret signed_int) then 
				error f.info ("Invalid return type for function "^f.node^", must be " ^ print_type (Tint(Signed,Int)))
			else 
				match args with
				|[] -> ()
				|_ -> 
					if (List.length args > 2 || List.length args == 1) then 
						error f.info ("Invalid number of arguments for function "^f.node^" ("^(string_of_int(List.length args))^" found, 0 or 2 expected)")
					else
						let t1,_ = (List.nth args 0) in
						let t2,_ = (List.nth args 1) in
						if not ((equals t1 signed_int)&&(equals t2 (Tpoint(Tpoint(Tint(Signed,Char)))))) then 
							error f.info ("Invalid arguments for function "^f.node^", "^print_type signed_int^" and "^print_type (Tpoint(Tpoint(Tint(Signed,Char))))^" expected")
		end;
		result
	with
		Not_found -> 
			Printf.printf "Main manquant\n";
			assert false
