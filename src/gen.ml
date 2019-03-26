open Core.Std
open Printf
open Ast

let (++) = List.append ;;
let count = ref (-1)
let tmp() = incr count; "v" ^ string_of_int !count
let total = ref 0
let fail = ref 0
let miss = ref 0
let hit = ref 0

module State = struct

	type const_or_var = int ref
	type assoc = op list
	type state = const_or_var * assoc * string

	let init () : state = (ref 0, [], "")
	let const_or_var s : const_or_var = match s with (x,_,_) -> x
	let assoc s : assoc = match s with (_,x,_) -> x
	let construct s : string = match s with (_,_,x) -> x

end 

let get_config conf = match conf with 
	| "assoc" -> Configure.default (false, true, true, true)
	| "comm" ->  Configure.default (true, false, true, true)
	| "cache" -> Configure.default (true, true, false, true)
	| "learn" -> Configure.default (true, true, true, false)
	| "default" -> Configure.conf


let time f x =
	let t = Unix.gettimeofday () in
    let fx = f x in
	((Unix.gettimeofday () -. t), fx)


let filt l p = 
	let rec f l = match l with
	| [] -> []
	| x :: xs -> if p x then [x] else f xs in 
	f l


let repeat ls n =
    let rec f l = function
        | 0 -> l
        | n -> f (List.rev_append ls l) (n-1) in
    if n < 0 then [] else List.rev (f [] n)


let distribute c l =
  let rec insert a1 a2 = function
    | [] -> a2
    | x::xs -> insert (x::a1) ((List.rev_append a1 (x::c::xs)) :: a2) xs in insert [] [c::l] l


let flatten ll = let rec go acc = function
    | [] -> List.rev acc
    | l :: r -> go (List.rev_append l acc) r in
    go [] ll


let (>>>) = 
	let rec iter acc m n = if n < m then acc else iter ( n :: acc ) m ( n-1 ) in 
	iter []


let rec split m i = match i with 
	| 1 -> [[m]]
	| _ -> let f j = List.map ~f:(fun l -> j :: l)(split (m-j) (i-1)) in flatten (List.map ~f:f (0 >>> m))


let rec split_com m i = 
	let rec is_sorted x = match x with
 		| [] -> true
  		| h::[] -> true
  		| h::h2::t -> if h >= h2 then is_sorted (h2::t) else false in
	List.rev (List.filter ~f:(fun x -> is_sorted x) (split m i))


let cartesian l l' = List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> [e;e']) l') l)


let enum_ty c = 
	let rec tys c = 
		if c <= 0 then 
			[List_t Int_t; List_t Bool_t; Int_t; Bool_t;] 
		else
		let list_types = List.map ~f:(fun x -> List_t(x))(tys (c-5)) in
		let tree_types = List.map ~f:(fun x -> Tree_t(x))(tys (c-5)) in
		let pair_types = List.map ~f:(fun [x;y] -> Pair_t(x,y)) (flatten (List.map ~f:(fun [x;y] -> cartesian (tys x) (tys y)) (split (c-5) 2))) in
		list_types ++ tree_types ++ pair_types in
	tys c 

	
let rec cost e = match e with
	| Num x -> 0
	| Bool b -> 0
	| Var v -> 0
	| Lambda (x,y) -> 1 + cost y
	| Let (x,y,z) -> 1 + cost y + cost z
	| If (x,y,z) -> 3 + cost x + cost y + cost z
	| App (x,t,z) -> 1 + cost x + cost z
	| Op (x,y) -> let rec ops o = (match o with
			| [] -> 0
			| x :: xs -> cost x + ops xs) in ops y + ((List.length y) - 1)
	| FoldR (x, y, z) -> 1 +cost x + cost y + cost z
	| FoldL (x, y, z) -> 1 +cost x + cost y + cost z
	| FoldT (x, y, z) -> 1 +cost x + cost y + cost z
	| Map (x, y) -> 1 + cost x + cost y
	| Filter (x, y) -> 1 + cost x + cost y
	| MapT (x, y) -> 1 + cost x + cost y
	| Pair (x, y) -> 1 + cost x + cost y
	| List(x) -> let rec lst l = (match l with
		| Empty -> 0
		| Cons (x, y) -> 1 + cost x + lst y) in lst x
	| Tree (x) -> let rec tr t = (match t with
		| Tree.Leaf -> 0
		| Tree.Node (x, y, z) -> 1 + cost x + tr y + tr z ) in tr x
	| _ -> failwith "error"


let rec cost_ty e = match e with
	| Int_t -> 0
	| Bool_t -> 0
	| Var_t x -> 0 
	| Empty_t -> 0
	| List_t x -> 5 + cost_ty x
	| Tree_t x -> 5 + cost_ty x
	| Arrow_t (x, y) -> 5 + cost_ty x + cost_ty y
	| Pair_t (x, y) -> 5 + cost_ty x + cost_ty y
	| _ -> failwith "error"


let size l = List.fold_left ~f:(fun acc _ -> acc + 1) ~init:0 l


let rec unfl l = match l with
	| x :: xs -> [[x]] ++ unfl xs
	| [] -> [] 


let rec n_cartesian_product x = 
	let cart ls = (match ls with
	| [] -> []
	| [l] -> List.map ~f:(fun i -> [i]) l
	| h :: t -> let rest = n_cartesian_product t in List.concat (List.map ~f:(fun i -> List.map ~f:(fun r -> i :: r) rest) h)) in cart x


let typ_cache = Hashtbl.create ~hashable:Hashtbl.Poly.hashable() ~size:1000
let cache = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () ~size:10000


let rec enum ?lrn_state:(lrn_state=0) ?lrn:(lrn=true) ?ch:(ch=true) ?assc:(assc=true) ?comm:(comm=true) ?lib:(lib=false) ?excl:(excl=[]) c typ vars state=
	let open Env in
	let open State in
	let open Tuple.T2 in
	let splits = split (c-1) 2 in

	let typ_cache size = 
		if ch = true then
			match Hashtbl.find typ_cache size with Some x -> x | None -> let res = enum_ty size in Hashtbl.add typ_cache ~key:size ~data:res; res
		else 
			enum_ty size in
	
	let get_vars_of_type typ env = 
		List.map ~f:(fun y -> Var(y))(List.filter (keys env) ~f:(fun x -> (match (lookup_exn env x) with (t, _ ) -> if t = typ then true else false))) in

	let enum_consts typ = 
		let nums = [0;1;2;3;4;5] in 
		let bools = [true; false;] in match typ with
		| Int_t -> (List.map ~f:(fun x-> Num(x)) nums)
		| Bool_t -> (List.map ~f:(fun x-> Bool(x)) bools)
		| List_t x -> [List(Empty)]
		| _ -> [] in

	let enum_ops c typ vars state =
		let gen_args t1 ops =
			let (op, max_args, com_op) = ops in 
				if (List.mem (assoc state) op && assc) then [] else

			let rec get_splits m c1 = 
				if c1 <= 0 then [] else 
				if (com_op = 0 && comm) then split_com m c1 else split m c1 ++ (get_splits m (c1-1)) in 

			let splits = get_splits (c-2) (c-1) in
			let zeros = repeat [0] (c-1) in

			let op_state = 
				if List.mem [Sub;Div;Mod;Eq_Int;Ls;LsEq;Grt;Geq;Eq_Bool;Neq_Bool;] op  then (ref 1, [], "op") else (ref 1, op :: (assoc state), "op") in 

			match max_args with
					| 0 -> (* 0 = inf args *)
						let args = 
							flatten ((List.map ~f:(fun x -> (n_cartesian_product (List.map ~f:(fun y -> enum y t1 vars op_state) x))) (splits))) in 								(* Argument lists comprised of sub programs that add up to current cost  *)
						let zero_args = 
							List.map ~f:(fun y -> flatten y) (n_cartesian_product ((List.map ~f:(fun x -> (unfl (enum x t1 vars op_state))) zeros))) in 							(* Argument lists comprised of sub programs only of cost 0 *)
						let zero_args = 
								flatten (List.map ~f:(fun y -> (flatten (List.map ~f:(fun x -> distribute y x) zero_args))) (enum_consts t1 ++ get_vars_of_type t1 vars)) in 	  	(* Distribute the constant across the arguments *)
						let args = 
							zero_args ++ args in List.map ~f:(fun x -> Op(op, x)) ((List.filter ~f:(fun x -> (List.length x) > 1)) args) 											(* Filter out results where there is only 1 operation argument *)
					| 1 -> 
						List.map ~f:(fun x -> Op(op, [x])) (enum (c-1) t1 vars op_state)
					| 2 -> match op with
						| Index ->  (List.map ~f:(fun [x;y] -> Op(Index, [x;y])) (flatten (List.map ~f:(fun [j;k] ->  (cartesian (enum j (List_t t1) vars op_state) (enum k Int_t vars op_state))) (split (c-1) 2))))
						| Branch -> (List.map ~f:(fun [x;y] -> Op(Branch, [x;y])) (flatten (List.map ~f:(fun [j;k] -> (cartesian (enum j t1 vars op_state) (enum k (List_t (Tree_t t1)) vars op_state))) (split (c-1) 2))))
						| Append -> (List.map ~f:(fun [x;y] -> Op(Append, [x;y])) (flatten (List.map ~f:(fun [j;k] -> (cartesian (enum j (List_t t1) vars op_state) (enum k t1 vars op_state))) (split (c-1) 2))))
						| Concat -> (List.map ~f:(fun [x;y] -> Op(Concat, [x;y])) (flatten (List.map ~f:(fun [j;k] -> (cartesian (enum j (List_t t1) vars op_state) (enum k (List_t t1) vars op_state))) (split (c-1) 2))))
						| _ -> []
					| _ -> [] in 
		match typ with
		| Int_t -> 		flatten (List.map ~f:(fun x -> (gen_args Int_t (x, 0, 0))) [Add;Mul;]) ++
						flatten (List.map ~f:(fun x -> (gen_args Int_t (x, 0, 1))) [Sub;Div;Mod]) ++ 
						flatten (List.map ~f:(fun x -> (gen_args (List_t Int_t) (x, 1, 0))) [Head]) ++
						flatten (List.map ~f:(fun x -> (gen_args Int_t (x, 2, 0))) [Index; Append]) ++
						flatten (List.map ~f:(fun x -> (gen_args (Tree_t Int_t) (x, 1, 0))) [Children; Value])
		| Bool_t -> 	flatten (List.map ~f:(fun x -> (gen_args Int_t (x, 0, 1))) [Eq_Int;Ls;LsEq;Grt;Geq]) ++ 
						flatten (List.map ~f:(fun x -> (gen_args Bool_t (x, 0, 1))) [Eq_Bool;Neq_Bool;]) ++ 
						flatten (List.map ~f:(fun x -> (gen_args Bool_t (x, 1, 1))) [Not]) ++ 
						flatten (List.map ~f:(fun x -> (gen_args (List_t Bool_t) (x, 1, 0))) [Head]) ++
						flatten (List.map ~f:(fun x -> (gen_args (Bool_t) (x, 2, 0))) [Index; Append])
		| List_t(t1) -> flatten (List.map ~f:(fun x -> (gen_args (List_t t1) (x, 1, 0))) [Tail;]) ++
						flatten (List.map ~f:(fun x -> (gen_args (Tree_t (List_t t1)) (x, 1, 1))) [Value;]) ++
						flatten (List.map ~f:(fun x -> (gen_args t1 (x, 2, 0))) [Concat; Append]) 
		| Tree_t(t1) -> flatten (List.map ~f:(fun x -> (gen_args t1 (x, 2, 0))) [Branch;]) ++
						flatten (List.map ~f:(fun x -> (gen_args (Tree_t t1) (x, 1, 0))) [Children; Value])
		| _ -> [] in 

	let enum_lambdas c typ vars state =
		let v1 = tmp() in
		let lam_state = (ref 1, (assoc state), "lam_body") in match typ with
		| Arrow_t(t1, t2) -> 
			(List.map ~f:(fun y -> Lambda(v1, y)) (enum (c-1) t2 (bind vars v1 (t1, Var(v1))) lam_state)) 
		| _ -> [] in

	let rec enum_lets c typ vars state = 
		let v = tmp() in
		let ps spl = 
			let [x;y;z] = spl in 
			let s_lam = (ref 1, (assoc state), "let_bind") in
			let s_prog = (ref 1, (assoc state), "let_body") in
			flatten(List.map ~f:(fun f -> (cartesian (enum x f vars s_lam) (enum y typ (bind vars v (f, Var(v))) s_prog))) (enum_ty z)) in
			match typ with 
			| t1 -> flatten (List.map ~f:(fun z -> (List.map ~f:(fun [x;y] -> Let(v,x,y))  (ps z))) (List.filter ~f:(fun [x;y;z] -> x >= 1 && y >= 1 ) (split (c-1) 3)))
			| _ -> [] in

	let enum_apps c typ vars state =
		let arg_state = (ref 1, (assoc state), "app_arg") in
 		let body_state = (ref 1, (assoc state), "app_body") in
		let ps spl = 
			let [x;y;z] = spl in
		 	flatten (List.map ~f:(fun f -> (List.cartesian_product (cartesian (enum x (Arrow_t(f,typ)) vars body_state) (enum y f vars arg_state)) [(Arrow_t(f,typ))])) (enum_ty z)) in 
		match typ with
		| t1 -> flatten (List.map ~f:(fun z -> (List.map ~f:(fun ([x;y],t) -> App(x,t,y)) (ps z))) (split (c-1) 3))
		| _ -> [] in 

	let enum_maps c typ vars state =
	let state_lam = (ref 0, (assoc state), "app_body") in
	let v1 = tmp() in match typ with
		| Arrow_t (List_t t1, List_t t2) -> 
			(List.map ~f:(fun x -> Lambda(v1, Map(x, Var(v1)))) (enum (c-1) (Arrow_t(t1, t2)) vars state_lam))
		| Arrow_t (Tree_t t1, Tree_t t2) ->
			(List.map ~f:(fun x -> Lambda(v1, MapT(x, Var(v1)))) (enum (c-1) (Arrow_t(t1, t2)) vars state_lam))
		| _ -> [] in

	let enum_filters c typ vars state = 
		let v1 = tmp() in
		let state_lam = (ref 0, (assoc state), "app_body") in
		match typ with
		| Arrow_t (List_t t1, List_t t2) -> 
			(List.map ~f:(fun x -> Lambda(v1, Filter(x, Var(v1)))) (enum (c-1) (Arrow_t(t1, Bool_t)) vars state_lam))
		| _ -> [] in

	let enum_folds c typ vars state = 
		let v1 = tmp() in
		let v2 = tmp() in
		let state_lam = (ref 0, (assoc state), "app_body") in
		let state_acc = (ref 0, (assoc state), "fold_acc") in
		let ps t1 t2 = flatten(List.map ~f:(fun [x;y] -> cartesian (enum x t1 (bind vars v2 (t2, Var(v2))) state_lam) (enum y t2 vars state_acc)) (split (c-1) 2)) in
		match typ with 
		| Arrow_t (List_t t1, t2) ->
			(List.map ~f:(fun [x;y] -> Lambda(v1, FoldL((Lambda(v2, x)), y, Var(v1)))) (ps (Arrow_t(t1, t2)) t2 )) ++ 
			(List.map ~f:(fun [x;y] -> Lambda(v1, FoldR((Lambda(v2, x)), y, Var(v1)))) (ps (Arrow_t(t1, t2)) t2 )) 
		| Arrow_t (Tree_t t1, t2) ->
			(List.map ~f:(fun [x;y] -> Lambda(v1, FoldT((Lambda(v2, x)), y, Var(v1)))) (ps (Arrow_t(t1, t2)) t2 )) 
		| _ -> [] in

	let enum_lists c typ vars state = 
	let list_state = (ref 1, (assoc state), "list") in match typ with 	
		| List_t t1 -> 
			let rec list_contents c1 = 
				if c1 = 0 then [Empty] else (List.map ~f:(fun (l,m) -> Cons(l, m)) (flatten (List.map ~f:(fun [x;y] -> (List.cartesian_product (enum x t1 vars list_state) (list_contents y))) (split (c1-1) 2))))
			in List.map ~f:(fun x -> List(x)) (list_contents (c))
		| _ -> [] in

	let enum_trees c typ vars state = 
		let tree_state = (ref 1, (assoc state), "tree") in match typ with 
		| Tree_t t1 -> 
			let rec tree_contents c1 = 
				if c1 = 0 then [Tree.Leaf] else (List.map ~f:(fun (l,(m,n)) -> Tree.Node(l, m, n)) (flatten (List.map ~f:(fun [x;y;z] -> (List.cartesian_product (enum x t1 vars tree_state) (List.cartesian_product (tree_contents y) (tree_contents z)))) (split (c1-1) 3))))
			in List.map ~f:(fun x -> Tree(x)) (tree_contents c)
		| _ -> [] in

	let enum_pairs c typ vars state = 
		let pair_state = (ref 1, (assoc state), "pair") in
		let ps t1 t2 = 
			flatten(List.map ~f:(fun [x;y] ->
				 cartesian (enum x t1 vars pair_state) (enum y t2 vars pair_state)) (split (c-1) 2)) in
		match typ with
		| Pair_t (t1, t2) -> 
			(List.map ~f:(fun [x;y] -> Pair(x, y)) (ps t1 t2)) 
		| _ -> [] in

	let enum_ifs c typ vars state =
		let if_state = (ref 1, (assoc state), "if_body") in
		let ps = (flatten(List.map ~f:(fun [x;y;z] -> n_cartesian_product [(enum x Bool_t vars if_state); (enum y typ vars if_state); (enum z typ vars if_state)]) (split (c-3) 3))) in
		List.map ~f:(fun [x;y;z] -> If(x,y,z)) ps in

	if c < 0 then [] else
	if c = 0 then (match !(const_or_var state) with
		| 0 -> (enum_consts typ) ++ (get_vars_of_type typ vars) 
		| 1 -> (get_vars_of_type typ vars)
		| 2 -> (enum_consts typ))
	else
	let che pr f = 
		if List.mem excl pr then [] else
		if ch = true then
			match Hashtbl.find cache (c, typ, vars, state, pr) with Some x -> incr hit; x | None -> incr miss; let res = f c typ vars state in Hashtbl.add cache ~key:(c, typ, vars, state, pr) ~data:res; res
		else 
			f c typ vars state in
	let ls = che "List" enum_lists in
	let trs = che "Tree" enum_trees in
	let lams = che "Lam" enum_lambdas in
	let lets = che "Let" enum_lets in
	let o = che "Ops" enum_ops in
	let ifs = che "Ifs" enum_ifs in
	let a = che "App" enum_apps in
	let map = che "Map" enum_maps in
	let fil = che "Fil" enum_filters in
	let fold = che "Fold" enum_folds in
	let pairs = che "Pair" enum_pairs in

	(match (lrn_state, lrn) with 
		| (1,true) -> map ++ fold ++ lams
		| (2,true) -> fil ++ fold ++ lams
		| (3,true) -> fold ++ lams
		| _ -> match construct state with 
			| "app_arg" -> a ++ pairs 
			| "app_body" -> lams ++ map ++ fil ++ fold ++ lets
			| "lam_body" -> a ++ map ++ fil ++ fold ++ pairs ++ o ++ ifs ++ ls ++ trs ++ lets
			| "let_bind" -> lams ++ map ++ fil ++ fold ++ o 
			| "let_body" ->  lams ++ lets
			| "fold_acc" -> a ++ pairs ++ ls ++ trs
			| "pair" -> a ++ pairs ++ ifs ++ o
			| "if_body" -> a ++ pairs ++ ifs 
			| "list" -> o ++ pairs
			| "tree" -> o ++ pairs
			| "op" ->  a ++ lams ++ map ++ fil ++ fold ++ pairs ++ o ++ ifs ++ ls ++ trs ++ lets
			| _ -> a ++ lams ++ map ++ fil ++ fold ++ pairs ++ o ++ ifs ++ ls ++ trs ++ lets )



let check env examples expr = let open Eval in let open Tuple.T2 in
	let rec f o = match o with
		| [] -> true 
		| x :: xs ->
				try ( if (eval (Env.bind env "IN" ((Infer.infer env (get1 x)), (get1 x))) (App(expr, Empty_t, Var("IN")))) = (get2 x) then f xs else false) with _ -> incr fail; false
	in incr total; f examples 


let rec inf l e f = match l with
	| [] -> failwith "Error: examples required "
	| x :: [] -> Infer.infer e (f x)
	| x :: xs  -> 
		let typ_of_current = Infer.infer e (f x) in
		let typ_of_next = Infer.infer e (f (List.hd_exn xs)) in
		if typ_of_current =  typ_of_next || (typ_of_current = Empty_t || typ_of_next = Empty_t) then 
			(if inf xs e f = Empty_t then typ_of_current else inf xs e f) 
		else failwith "Error: inconsistent types in examples." 


let synthesise examples ?conf:(conf="default") = 
	let config = get_config conf in
	let (assoc, comm, caching, lrn, libs, laziness, excl) = (config.assoc, config.commutativity, config.cache, config.learning, config.libraries, config.laziness, config.exclude) in
	let open Tuple.T2 in
	let typ = Arrow_t((inf examples (Env.empty()) get1), (inf examples (Env.empty()) get2)) in
	let learning = if lrn = true then Learn.learn examples typ else 0 in
	total := 0; 
	fail := 0;
	Hashtbl.clear cache;
	let rec gen w = 
		let v0 = tmp() in
		let v1 = tmp() in
		let v2 = tmp() in
		let prog = match typ with 
			| Arrow_t(Pair_t(t1, t2), t3) -> List.map ~f:(fun x -> 
					Lambda (v0, (Let(v1, (Op(Fst,[Var(v0)])), (Let (v2, (Op(Snd, [Var(v0)])), x)))))) (enum w t3 (Env.bind (Env.bind (Env.empty()) v1 (t1, (Var("v1")))) v2 (t2, (Var("v2")))) (State.init()) ~lrn_state:learning ~ch:caching ~assc:assoc ~comm:comm ~lib:libs ~excl:excl )
			| _ ->  enum w typ (Env.empty()) (State.init()) ~lrn_state:learning ~ch:caching ~assc:assoc ~comm:comm ~lib:libs ~excl:excl  in
		let x = filt prog (check (Env.empty()) examples) in 
		if x = [] then gen (w+1) 	else List.hd_exn x in
	let (t,res) = time gen 0 in (res, !total, (!total - !fail), t)


