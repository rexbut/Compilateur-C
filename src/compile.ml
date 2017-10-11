(* Comparaisons en unsigned pour les double *)
open Ast
open Amd64
open Typing

(* Initialisation des environnements *)
let string_env = Hashtbl.create 17
let double_env = Hashtbl.create 17

(* Initialisation des registres *)
let int_registers = [rdi; rsi; rdx; rcx; r8; r9]
let double_registers = [xmm0; xmm1; xmm2; xmm3; xmm4; xmm5; xmm6; xmm7]

(* Permet de créer un nouveau label *)
let new_label =
  let c = ref 0 in
  fun s -> incr c; Printf.sprintf "__label__%s_%05i" s !c

(* Permet de connaitre la taille d'un type *)
let size_of t =
	match t with
	| Tvoid -> 0
	| Tint(_, i) ->
		begin
			match i with 
			| Char	-> 1
			| Short	-> 2
			| Int	-> 4
			| Long	-> 8
		end
	| Tdouble | Tpoint _  | Tnull -> 8
	| Tstruct _ -> 0 (* TODO struct *)
	
(* *)
let align_of t =
	match t with
	Tstruct _ -> assert false
	| _ -> size_of t

(* Permet d'avoir un nombre divisible par 8 *)
let round8 n =
	if n mod 8 = 0 then n else (((n/8)+1)*8)
		
(* Permet de récupérer le bon registre par rapport à un type *)
let reg_size_of t =
	match size_of t with
	| 1 -> `b
	| 2 -> `w
	| 4 -> `l
	| _ -> `q

(* Permet de récupérer le bon registre par rapport à un type *)
let reg_size_of_nb t =
	match size_of t with
	| 2 -> `w
	| 4 -> `l
	| _ -> `q
	

let log n = 
	match n with
	| 1 -> 0
	| 2 -> 1
	| 4 -> 2
	| 8 -> 3
	| _ -> assert false
	
(* Permet de savoir si le type est signé *)	
let is_signed t = 
	match t with
	|Tint(Signed, _) | Tpoint _ |Tnull -> true
	| _ -> false

(* Retourne le langage assembleur d'un cast *)
let compile_cast tfrom tto =
	let tfrom = if tfrom = Tnull then Tint(Unsigned, Long) else tfrom in
	let tto = if tto = Tnull then Tint(Unsigned, Long) else tto in
	let size_tfrom = size_of tfrom in
	let size_tto = size_of tto in
	match tfrom, tto with
		|(Tvoid | Tstruct _), _ -> assert false
		| _ , (Tvoid | Tstruct _) -> assert false
		| _ when size_tfrom = size_tto -> nop
		| _ when size_tto < size_tfrom ->
			let mask = (1 lsl (size_tto * 8)) - 1 in
			andq ~$mask ~%r10
		| Tint (Signed, Char), Tint(_, Short) -> movsbw ~%r10b ~%r10w
		| Tint (Unsigned, Char), Tint(_, Short) -> movzbw ~%r10b ~%r10w
		| Tint (Signed, Char), Tint(_, Int) -> movsbl ~%r10b ~%r10d
		| Tint (Unsigned, Char), Tint(_, Int) -> movzbl ~%r10b ~%r10d
		| Tint (Signed, Char), Tint(_, Long) -> movsbq ~%r10b ~%r10
		| Tint (Unsigned, Char), Tint(_, Long) -> movzbq ~%r10b ~%r10
		| Tint (Signed, Short), Tint(_, Int) -> movswl ~%r10w ~%r10d
		| Tint (Unsigned, Short), Tint(_, Int) -> movzwl ~%r10w ~%r10d
		| Tint (Signed, Short), Tint(_, Long) -> movswq ~%r10w ~%r10
		| Tint (Unsigned, Short), Tint(_, Long) -> movzwq ~%r10w ~%r10
		| Tint (Signed, Int), Tint(_, Long) -> movslq ~%r10d ~%r10
		| Tint (Unsigned, Int), Tint(_, Long) -> andq ~$0xffffffff ~%r10
		| Tdouble, (Tnull | Tint _ | Tpoint _) -> 
			cvttsd2siq ~%xmm0 ~%r10
		| (Tint _ | Tpoint _ | Tnull), Tdouble -> 
			cvtsi2sdq ~%r10 ~%xmm0 ++
			movq ~%xmm0 ~%r10
		| _ -> failwith ("type : "^(print_type tfrom)^ " : "^ (print_type tto))

(* Retourne le bon registre *)
let rec assign_regs env args iregs dregs (d_acc, code_acc) =
	match args, iregs, dregs with
	| [], _, _ -> d_acc, code_acc
	| e :: _, _, [] when e.info = Tdouble -> assert false
	| e :: _, [], _ -> assert false
	| e :: next_args, _, dreg :: next_dregs when e.info = Tdouble ->
		assign_regs env next_args iregs next_dregs
		(1 + d_acc, code_acc ++ compile_expr env e ++ popd ~%dreg)
	| e :: next_args, ireg :: next_iregs, _ ->
		assign_regs env next_args next_iregs dregs
		(d_acc, code_acc ++ compile_expr env e ++ popq ~%ireg)

(* Transforme une constante en langage assembleur *)
and compile_const c = 
	match c with
	| Cint(_, Long, i) ->
		movabsq i ~%r10
	| Cint(_, _, i) ->
		movl ~$(int_of_string i) ~%r10d
	| Cstring s ->
		let label =
			try
				Hashtbl.find string_env s
			with Not_found ->
				let lab = new_label "string" in
				Hashtbl.add string_env s lab;
				lab
		in
		mov ~:label ~%r10
	| Cdouble d -> 
		let label =
			try
				Hashtbl.find double_env d
			with Not_found ->
				let lab = new_label "double" in
				Hashtbl.add double_env d lab;
				lab
		in 
		mov ~:label ~%r10 ++ mov (addr ~%r10) ~%r10

(* Transforme une expression en langage assembleur et met le résultat dans un registre *)
and compile_expr_reg env e =
	match e.node with
	| Econst c ->
		compile_const c
	| Ecast (t, e0) ->
		compile_expr_reg env e0
		++ compile_cast e0.info t
	| Eident _ | Eunop(Ref, _) | Egetstr _ ->
		let reg10 = r10_ (reg_size_of e.info) in
		compile_lvalue_reg env e ++
		mov (addr ~%r10) ~%reg10
	| Eunop(Addr, e) ->
		compile_lvalue_reg env e
	| Eassign (e1, e2) ->
		let e1_code = compile_lvalue_reg env e1 in
		let e2_code = compile_expr env e2 in
		let reg = r11_ (reg_size_of e1.info) in
		e2_code ++
		e1_code ++ 
		popq ~%r11 ++
		mov ~%reg (addr ~%r10)++
		movq ~%r11 ~%r10		
	| Ebinop(binop, e1, e2)	-> 
		(* op ra rb <=> rb <- rb op ra *)
		begin
			let e2_code = compile_expr env e2 in
			let e1_code = compile_expr_reg env e1 in
			(* r10 deuxième opérande, r11 première *)
			let regE1 = r10_ (reg_size_of_nb e.info) in
			let regE2 = r11_ (reg_size_of_nb e.info) in
			let reg12 = r12_ (reg_size_of_nb e.info) in
			e2_code ++ 
			e1_code ++ 
			popq ~%r11 ++
			match binop with
			| Plus when equals e1.info Tdouble ->
				movq ~%r10 ~%xmm0 ++
				movq ~%r11 ~%xmm1 ++
				addsd ~%xmm0 ~%xmm1 ++
				movq ~%xmm1 ~%r10
			| Plus ->
				(match (e1.info, e2.info) with
				| (Tpoint sub1, _ ) ->
					let size = (size_of sub1) in
					mov ~$size ~%r12 ++
					imul ~%reg12 ~%regE2
				| (_, Tpoint sub2) ->
					let size = (size_of sub2) in
					mov ~$size ~%r12 ++
					imul ~%reg12 ~%regE1
				| _ -> nop)++
				add ~%regE2 ~%regE1
			| Minus when equals e1.info Tdouble ->
				movq ~%r10 ~%xmm1 ++
				movq ~%r11 ~%xmm0 ++
				subsd ~%xmm0 ~%xmm1 ++
				movq ~%xmm1 ~%r10
			| Minus ->
				(match (e1.info, e2.info) with
				| (Tpoint sub1, Tpoint _) ->
					let dec = log (size_of sub1) in
					sub ~%regE2 ~%regE1 ++
					sarq ~$dec ~%r10
				| (Tpoint sub1, _ ) ->
					let size = (size_of sub1) in
					mov ~$size ~%r12 ++
					imul ~%reg12 ~%regE2
				| _ -> sub ~%regE2 ~%regE1)
			| Mult when equals e1.info Tdouble ->
				movq ~%r10 ~%xmm0 ++
				movq ~%r11 ~%xmm1 ++
				mulsd ~%xmm0 ~%xmm1 ++
				movq ~%xmm1 ~%r10
			| Mult ->
				imul ~%regE2 ~%regE1
			| (And|Or) ->
				cmpq ~$0 ~%r10 ++
				setne ~%r10b ++
				cmpq ~$0 ~%r11 ++
				setne ~%r11b ++
				(match binop with
				| And ->
					andb ~%r11b ~%r10b
				| Or ->
					orb ~%r11b ~%r10b
				| _ -> nop) ++
				movzbq ~%r10b ~%r10
			| (Eq|Neq|Lt|Le|Gt|Ge) when (equals e1.info Tdouble || equals e2.info Tdouble) ->
				begin
					movq ~%r10 ~%xmm0 ++
					movq ~%r11 ~%xmm1 ++
					ucomisd ~%xmm1 ~%xmm0 ++
					match binop with 
					| Eq -> sete ~%r10b
					| Neq -> setne ~%r10b
					| Lt -> setb ~%r10b
					| Le -> setbe ~%r10b
					| Gt -> seta ~%r10b
					| Ge -> setae ~%r10b
					| _ -> nop
				end ++
				movzbl ~%r10b ~%r10d
			| (Eq|Neq|Lt|Le|Gt|Ge) ->
				cmp ~%regE2 ~%regE1++
				(if is_signed e1.info then 
					begin
						match binop with 
						| Eq -> sete
						| Neq -> setne
						| Lt -> setl
						| Le -> setle
						| Gt -> setg
						| Ge -> setge
						| _ -> assert false
					end
				else
					begin
						match binop with
						| Eq -> sete
						| Neq -> setne
						| Lt -> setb
						| Le -> setbe
						| Gt -> seta
						| Ge -> setae
						| _ -> assert false
					end) ~%r10b ++
				movzbl ~%r10b ~%r10d				
			| Div when equals e1.info Tdouble ->
				movq ~%r10 ~%xmm1 ++
				movq ~%r11 ~%xmm0 ++
				divsd ~%xmm0 ~%xmm1 ++
				movq ~%xmm1 ~%r10
			| (Div|Mod) ->
				let rsize = reg_size_of_nb e1.info in
				let ra = rax_ rsize in
				let rd = rdx_ rsize in
				mov ~%regE1 ~%ra ++
				(if is_signed e1.info then
					(if rsize = `q then
						cqto ++ idivq ~%r11
					else
						cltd ++ idivl ~%r11d)
				else
					xor ~%rd ~%rd++
					(if rsize = `q then
						cqto ++ divq ~%r11
					else
						cltd ++ divl ~%r11d)
				) ++
				(if binop = Div then mov ~%ra ~%regE1 else mov ~%rd ~%regE1)
		end
	| Eunop ((Pre_dec | Pre_inc) as unop, e0) ->
		begin
			let e0_code = compile_lvalue_reg env e0 in
			let reg10 = r10_ (reg_size_of_nb e.info) in
			let reg11 = r11_ (reg_size_of_nb e.info) in
			e0_code ++ 
			mov (addr ~%r10) ~%reg11 ++
			(match unop with
			| Pre_dec ->
				dec ~%reg11
			| Pre_inc ->
				inc ~%reg11
			| _ -> nop) ++
			mov ~%reg11 (addr ~%r10)++
			mov ~%reg11 ~%reg10
		end
	| Eunop ((Post_dec | Post_inc) as unop, e0) ->
		begin
			let e0_code = compile_lvalue_reg env e0 in
			let reg10 = r10_ (reg_size_of_nb e.info) in
			let reg11 = r11_ (reg_size_of_nb e.info) in
			let reg12 = r12_ (reg_size_of_nb e.info) in
			e0_code ++ 
			mov (addr ~%r10) ~%reg11 ++
			mov ~%reg11	~%reg12 ++
			(match unop with
			| Post_dec ->
				dec ~%reg11
			| Post_inc ->
				inc ~%reg11
			| _ -> nop) ++
			mov ~%reg11 (addr ~%r10)++
			mov ~%reg12 ~%reg10
		end
	| Eunop (unop, e0) ->
		begin  
			match unop with
			| Uminus ->
				let e0_code = compile_expr_reg env e0 in
				e0_code ++
				(if equals e0.info Tdouble then
					(* Soustraction *)
					movq ~%r10 ~%xmm0 ++
					xor ~%r10 ~%r10 ++
					cvtsi2sdq ~%r10 ~%xmm1 ++
					subsd ~%xmm0 ~%xmm1 ++
                    movq ~%xmm1 ~%r10
				else
					let reg10 = r10_ (reg_size_of_nb e.info) in
					neg ~%reg10)
			| Not ->
				let e0_code = compile_expr_reg env e0 in
				e0_code ++
				(if equals e0.info Tdouble then
					movq ~%r10 ~%xmm0 ++
					xor ~%r10 ~%r10 ++
					cvtsi2sdq ~%r10 ~%xmm1 ++
                    ucomisd ~%xmm1 ~%xmm0
				else
					cmpq ~$0 ~%r10)++
				sete ~%r10b ++
				movzbl ~%r10b ~%r10d
			| Uplus ->
				compile_expr_reg env e0
			| Addr -> 
				compile_lvalue_reg env e0
			| _ -> nop
		end
	| Ecall (f, params) ->
		let tret,_,_, local = Hashtbl.find fun_env f.node in
		if not local then
			let n_double, arg_code = 
				assign_regs env params int_registers double_registers (0, nop)
			in
			arg_code ++
			pushq ~%rsp ++
			pushq (addr ~%rsp) ++
			andq ~$(-16) ~%rsp ++
			movq ~$(n_double) ~%rax++
			call f.node ++
			movq (addr ~ofs:8 ~%rsp) ~%rsp ++
			mov ~%rax ~%r10
		else
			let size_ret = round8 (size_of tret) in
		let arg_size, arg_code =
			List.fold_left (fun (a_size, a_code) e ->
				(a_size + round8 (size_of e.info),
				compile_expr env e ++ a_code)
			) (0, nop) params
		in
		subq ~$size_ret ~%rsp ++
		arg_code ++
		call f.node ++
		addq ~$arg_size ~%rsp ++
		if (tret <> Tvoid) then popq ~%r10 else	nop
	| Esizeof t	-> 
		let size = size_of t in
		mov ~$size ~%r10
	|_ -> compile_lvalue_reg env e
	
and compile_lvalue_reg env e =
	match e.node with
	| Eident e ->
		begin
			try	
				let offset = Env.find e.node env in
				leaq (addr ~%rbp ~ofs:offset) ~%r10
			with Not_found ->
				movq ~:(e.node) ~%r10
		end
	| Eunop(Ref, e0) -> 
		compile_expr_reg env e0 
	| Egetarr (e1, e2) -> 
		let e1_code = compile_lvalue_reg env e1 in
		let e2_code = compile_expr env e2 in
		let size = size_of e1.info in
		e2_code ++
		e1_code ++
		popq ~%r11 ++
		imulq ~$size ~%r11 ++
		subq ~%r11 ~%r10
	| Egetstr (e, i) -> nop
	| _ -> failwith "todo2"
	
(* Transforme une expression en langage assembleur*)
and compile_expr env e =
	match e.info with
	| Tstruct _ -> assert false
	| Tvoid -> compile_expr_reg env e
	| t when size_of t = 8 ->
		compile_expr_reg env e ++ pushq ~%r10
	| t ->
		let n = size_of t in
		let mask = (1 lsl (n*8)) -1 in
		compile_expr_reg env e ++
		andq ~$mask ~%r10 ++
		pushq ~%r10

(* Transforme une expression en langage assembleur et supprime le résultat *)
and compile_clean_expr env e =
	let ecode = compile_expr env e in
	ecode ++ (if e.info = Tvoid then nop else popq ~%r10)

(* Transforme une instruction en langage assembleur *)
let rec compile_instr lab_fin rbp_offset env i = 
	match i.node with
	| Sskip -> rbp_offset, nop
	| Sexpr e -> rbp_offset, compile_clean_expr env e
	| Sbloc b -> compile_block lab_fin rbp_offset env b
	| Sreturn oe ->
		rbp_offset, (
			match oe with
			| None -> nop
			| Some e -> compile_expr env e
		) ++ jmp lab_fin
	| Sif (e, i1, i2) ->
		begin
			let e_code = compile_expr_reg env e in
			let i1_offset, i1_code = compile_instr lab_fin rbp_offset env i1 in
			let i2_offset, i2_code = 
				match i2 with
					| None -> 0, nop
					| Some i -> 
						compile_instr lab_fin rbp_offset env i
			in
			let else_label = new_label "else"
			and end_label  = new_label "end"
			in
			let code =
				e_code ++
				test ~%r10 ~%r10 ++
				je else_label ++
				i1_code ++
				jmp end_label ++
				label else_label ++
				i2_code ++
				label end_label
			in ((min i1_offset i2_offset), code)
		end
	| Sfor (e1, e2, e3, i) ->
		let e1_code = List.fold_left (fun (acode) e ->
			let ecode = compile_clean_expr env e in
			(acode++ecode)
		) (nop) e1 
		and e3_code = List.fold_left (fun (acode) e ->
			let ecode = compile_clean_expr env e in
			(acode++ecode)
		) (nop) e3
		and i_offset, i_code = compile_instr lab_fin rbp_offset env i
		and if_label = new_label "if"
		and end_label  = new_label "end"
		in
		match e2 with
		| None -> 
			let	code =
				e1_code ++
				label if_label ++
				i_code ++
				e3_code ++
				jmp if_label
			in (i_offset, code)
		| Some e2 -> 
			let e2_code = compile_expr_reg env e2 
			in
			let	code =
				e1_code ++
				label if_label ++
				e2_code ++
				test ~%r10 ~%r10 ++
				je end_label ++
				i_code ++
				e3_code ++
				jmp if_label ++
				label end_label
			in (i_offset, code)

(* Transforme un bloc en langage assembleur *)
and compile_block lab_fin rbp_offset env (var_decls, instrs) = 
	let new_offset, new_env, debug =
		List.fold_left (fun (aoffset, aenv, debug) (t, x) ->			
			let aenv = Env.add x.node aoffset aenv in
			let aoffset = aoffset - round8 (size_of t) in
			let debug = debug ++ comment (Printf.sprintf "local: %s rbp[%d]" x.node aoffset) in
			(aoffset, aenv, debug)
		) (rbp_offset, env, nop) var_decls
	in
	List.fold_left (fun ( aoffset, acode) i ->
		let ioffset, icode = compile_instr lab_fin new_offset new_env i in
		(min ioffset aoffset, acode++icode)
	) (new_offset, nop) instrs
	
(* Transforme une déclaration en langage assembleur *)
let compile_decl (atext, adata) d =
	match d with
	| Dtype _ ->
		atext, adata
	| Dvar (t, id) ->
		atext,
		let n = size_of t in
		let a = align_of t in
		label id.node ++
		align a ++
		space n ++
		adata
	| Dfun (_, _, _, None) ->
		atext, adata
	| Dfun (tret, f, params, Some body) ->
		let last_offset, env =
			List.fold_left (fun (aoffset, aenv) (t, x) ->
				let aoffset = aoffset + round8 (size_of t) in
				let aenv = Env.add x.node aoffset aenv in
				(aoffset, aenv)
			) (8, Env.empty) params
		in
		let ret_offset = last_offset + round8 (size_of tret) in
		let label_fin = f.node ^"_fin" in
		let max_rbp_offset, body_code = compile_block label_fin (-8) env body in
		(* max_rbp_offset est négatif (multiple de -8) *)
		let code =
			glabel f.node ++
			comment("On rentre dans la fonction " ^ f.node) ++
			pushq ~%rbp ++
			mov ~%rsp ~%rbp ++
			addq ~$max_rbp_offset ~%rsp ++
			body_code ++
			
			label label_fin ++
			(if (tret <> Tvoid) then
				if f.node = "main" then 
					popq ~%rax
				else
					popq ~%r10 ++
					mov ~%r10 (addr ~ofs:ret_offset ~%rbp)
			else nop) ++
			mov ~%rbp ~%rsp ++
			popq ~%rbp ++
			ret
		in
			atext ++ code, adata
	
(* Transforme un programme en langage assembleur *)
let compile_prog p =
	let text, data =
		List.fold_left compile_decl (nop, nop) p
	in
	let data = Hashtbl.fold (fun str lbl a_data ->
		a_data ++
		label lbl ++
		string str
	) string_env data
	in
	let data = Hashtbl.fold (fun dbl lbl a_data ->
		a_data ++
		label lbl ++
		ddouble [dbl]
	) double_env data
	in
	{
		text = text;
		data = data
	}