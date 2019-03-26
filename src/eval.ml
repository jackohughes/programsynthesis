open Core.Std
open Printf
open Ast

let rec eval env expr = 
	let eval_all exprs = List.map ~f:(fun e -> eval env e) exprs in 
	match expr with
		| Num x -> Num x
		| Bool x -> Bool x
		| Var x -> (match Env.lookup env x with
        	| Some (x,y) -> (eval env y)
        	| None -> failwith "error: no binding exists")
		| Let (x,y,z) -> 
			let env' = Env.bind env x (Int_t, (y)) in
			eval env' z
		| Lambda (arg, body) -> Closure (arg, body, env)
		| App (x,t,y) -> (match eval env x with
			| Closure (arg, body, c_env) ->
				let y' = eval env y in 
				let c_env' = (Env.bind c_env arg (Int_t, y')) in eval c_env' body	
			| _ -> failwith "error: illegal expression")
		| Op (x,y) -> 
			let rec nums_to_ints l = match l with 
				| Num(i1) :: xs -> i1 :: (nums_to_ints xs)
				| [] -> [] in 
			(match eval_all y with 
			| Num(i1) :: [] -> Num(i1)
			| Bool(b1) :: [] -> (match x with 
				| Not -> Bool(not b1)
				| _ -> Bool(b1))
			| List l :: [] -> (match x with
				| Head -> (match l with
					| Cons (x,y) -> x
					| _ -> failwith "exn: empty list")
				| Tail -> (match l with
					| Cons (x,y) ->  List(y)
					| _ -> failwith "exn: list too short")
				| _ -> failwith "error: incorrect input")
			| Tree t :: [] -> (match x with 
				| Value -> (match t with (Tree.Node (t, _, _)) -> t | _ -> failwith "error")
				| Children -> (match t with
             	 	| (Tree.Leaf) -> Tree (Tree.Leaf)
            		| (Tree.Node (_, l, r)) -> List(Cons((Tree(l)), (Cons((Tree(r)), Empty)))))
				| _ -> failwith "error: incorrect input")
			| Num(i1) :: xs -> 
				let f_a p = Bool(List.for_all (nums_to_ints (eval_all xs)) p) in
				(match x with 
				| Add -> Num(i1 + (match (eval env (Op(x,xs))) with Num i2 -> i2))
				| Sub -> Num(i1 - (match (eval env (Op(x,xs))) with Num i2 -> i2))
				| Mul -> Num(i1 * (match (eval env (Op(x,xs))) with Num i2 -> i2))
				| Div -> Num(i1 / (match (eval env (Op(x,xs))) with Num i2 -> i2))
				| Mod -> Num(i1 % (match (eval env (Op(x,xs))) with Num i2 -> i2))
				| Eq_Int -> (let pred c = (c = i1) in f_a pred)
				| Neq_Int -> (let pred c = (c <> i1) in f_a pred)
				| Grt -> (let pred c = (c > i1) in f_a pred)
				| Geq -> (let pred c = (c >= i1) in f_a pred)
				| Ls -> (let pred c = (c < i1) in f_a pred)
				| LsEq -> (let pred c = (c <= i1) in f_a pred))
			| Bool(b1) :: xs -> (match x with 
				| Eq_Bool -> Bool(b1 = (match (eval env (Op(x,xs))) with Bool b2 -> b2))
				| Neq_Bool -> Bool(b1 <> (match (eval env (Op(x,xs))) with Bool b2 -> b2)))
			| List l1 :: List l2 :: xs -> (match x with 
				| Concat -> 
					let rec concat l r = match l with
						| Cons(x, Empty) -> Cons(x, r)
						| Cons(x, y) -> Cons(x, (concat y r)) in
						List(concat l1 l2) 
				| _ -> failwith "error")
			| List l1 :: i1 :: [] -> (match x with
				| Index -> 
					let rec index l i = (match l with
						| Cons (x,y) -> if i = 0 then x else index y (i-1) 
						| _ -> failwith "error") in 
					let i = (match i1 with 
						| Num n -> n
						| _ -> failwith "error") in index l1 i
				| Append -> let rec append l i = (match l with
					| Cons (x,Empty) -> Cons (x,Cons(i, Empty))
					| Cons (x,y) -> Cons(x, (append y i)) 
					| Empty -> Cons(i, Empty)) in List (append l1 i1)
				| _ -> failwith "error: incorrect input")
			| Pair (l, r) :: [] -> (match x with
				| Fst -> eval env l
				| Snd -> eval env r) 
			| t1 :: List l1 :: [] -> (match x with 
				| Branch -> (match l1 with 
					| (Cons(Tree(x1), Cons(Tree(y1), Empty))) -> Tree(Tree.Node(t1,x1,y1)) 
					| _ -> failwith "error: empty list")
				| _ -> failwith "error: incorrect input")
			| _ -> failwith "error: incorrect input")
		| If (x,y,z) ->
			let then_branch = eval env y in
			let else_branch = eval env z in
			(match eval env x with 
				| Bool b -> if b then then_branch else else_branch
				| _ -> failwith "error")
		| List l ->
			let rec eval_list env l = (match l with
			| Cons (x,y) -> (Cons((eval env x), (eval_list env y)))
			| Empty -> Empty) in List(eval_list env l)
		| Tree t -> Tree(Tree.map ~f:(fun e -> eval env e) t)
		| Pair (x, y) -> Pair ((eval env x), (eval env y))
		| Map (f, l) -> (match ((eval env f), (eval env l)) with
				| (f1, List(a)) -> let rec apply_map env l2 = (match l2 with 
					| Cons (x,y) -> (Cons((eval env (App(f,Empty_t,x))), (apply_map env y)))
					| Empty -> Empty) in List(apply_map env a)
				| _ -> failwith "error")
		| MapT(f, t) -> (match ((eval env f), (eval env t)) with
				| (f1, Tree(a)) -> let rec apply_mapt env t2 =(match t2 with
					| Tree.Node(x,y,z) -> (Tree.Node((eval env (App(f,Empty_t,x))), (apply_mapt env y), (apply_mapt env z)))
					| Tree.Leaf -> Tree.Leaf) in Tree(apply_mapt env a)
				| _ -> failwith "error")
		| Filter (f, l) -> (match (f, eval env l) with
				| (Lambda(_,_), List a) -> let rec apply_filt env l2 = (match l2 with 
					| Cons (x,y) -> let eq = (App(f,Empty_t,x)) in (match eval env eq with
						| Bool(true) -> (Cons((eval env x), (apply_filt env y)))
						| _ -> (apply_filt env y))
					| Empty -> Empty) in List(apply_filt env a)
				| _ -> failwith "error")
		| FoldL (f, ac, l) -> (match (f, (eval env ac), (eval env l)) with
				| (Lambda(_,Lambda(_,_)), _, List(a)) -> let rec apply_foldl env acc l2 = (match l2 with
					| Cons (x,y) -> (apply_foldl env (eval env (App(App(f, Empty_t,x), Empty_t,acc))) y)
					| Empty -> acc) in apply_foldl env ac a
				| _ -> failwith "error")
		| FoldR (f, ac, l) -> (match (f, (eval env ac), (eval env l)) with
				| (Lambda(_,Lambda(_,_)), _, List(a)) -> let rec apply_foldr env acc l2 = (match l2 with
					| Cons (x,y) -> (eval env (App(App(f, Empty_t,x), Empty_t, (apply_foldr env acc y))))
					| Empty -> acc) in apply_foldr env ac a
				| _ -> failwith "error")
		| FoldT (f, ac, t) -> (match (f, (eval env ac), (eval env t)) with
				| (Lambda(_, Lambda(_,_)), _, Tree(a)) -> let rec apply_foldt env acc t2 = (match t2 with
					| Tree.Node (n,l,r) -> 
						let acc'' = apply_foldt env acc r in
						let acc'  = apply_foldt env acc'' l in
						(eval env (App(App(f,Empty_t, n),Empty_t, acc'))) 	
					| Tree.Leaf -> acc) in apply_foldt env ac a
				| _ -> failwith "error")
		| _ -> failwith "error"


