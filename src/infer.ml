open Core.Std
open Ast


let count: int ref = ref (-1)
let fresh_t() = incr count; "T" ^ string_of_int !count


let rec constraints env t = 

	let op_constraints t = match t with
	| Add | Sub | Mul | Div | Eq_Int | Neq_Int | Grt | Geq | Ls | LsEq -> (Int_t, [Int_t])
	| Eq_Bool | Neq_Bool | Not -> (Bool_t, [Bool_t])
	| Index -> let t1 = Var_t(fresh_t()) in (t1, [List_t(t1); Int_t])
	| Branch -> let t1 = Var_t(fresh_t()) in (Tree_t(t1), [List_t(t1); Tree_t(t1)])
	| Head -> let t1 = Var_t(fresh_t()) in (t1, [List_t(t1)])
	| Tail -> let t1 = Var_t(fresh_t()) in (List_t(t1), [List_t(t1)])
	| Value -> let t1 = Var_t(fresh_t()) in (t1, [Tree_t(t1)])
	| Children -> let t1 = Var_t(fresh_t()) in (List_t(Tree_t(t1)), [Tree_t(t1)]) in

	let rec op_args_constraints env t s = match t with
	| x :: [] -> constraints env x
	| x :: xs ->
		let (tx, tr) = op_constraints s in
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = op_args_constraints env xs s in
		let newconstr = [(t1, t2)] in
		(tx, (List.concat [newconstr; constr1; constr2;])) in

	let rec tree_constraints env t = let open Tree in match t with
	| Leaf -> (Empty_t, [])
	| Node (x,y,z) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = tree_constraints env y in
		let (t3, constr3) = tree_constraints env z in
		let newconstr = [(t1, t2); (t1, t3);] in
		(Tree_t(t1), (List.concat [newconstr; constr1; constr2; constr3;])) in
	
	let rec list_constraints env t = match t with
	| Empty -> (Empty_t, [])
	| Cons (x, y) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = list_constraints env y in
		let newconstr = [(t1, t2)] in
		(List_t(t1), (List.concat [newconstr; constr1; constr2])) in

	match t with
	| Var x -> let t1 = (match Env.lookup_exn env x with (l,m) -> l) in (t1, [])
	| Num x -> (Int_t, [])
	| Bool x ->  (Bool_t, [])
	| If (x,y,z) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env y in
		let (t3, constr3) = constraints env z in
		let newconstr = [(t1, Bool_t); (t2,t3);] in
		(t3, (List.concat [newconstr; constr1; constr2; constr3;]))
	| Lambda (x,y) -> 
		let x1 = fresh_t() in
		let t1 = Var_t(x1) in
		let (t2, constr1) = constraints (Env.bind env x (t1, Var(x1))) y in
		(Arrow_t(t1, t2), constr1)
	| App (x,t,y) ->
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env y in
		let tx = fresh_t() in
		let newconstr = [(t1,Arrow_t(t2,Var_t(tx)))] in 
		((Var_t(tx)), (List.concat [newconstr; constr1; constr2;]))
	| Let (x,y,z) -> 
		let (ty, constr1) = constraints env y in
		let x1 = fresh_t() in
		let v2 = Var_t(x1) in
		let (tz, constr2) = constraints (Env.bind env x (v2, Var(x1))) z in
		(tz, (List.concat [constr1; constr2;]))
	| Op (x, y) -> op_args_constraints env y x
	| List x -> list_constraints env x
	| Tree x -> tree_constraints env x
	| Pair (x, y) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env y in
		((Pair_t(t1, t2)), (List.concat [constr1; constr2]))
	| Map (x, y) ->
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env y in
		(Arrow_t(t2, match t1 with Arrow_t(l, m) -> List_t m), (List.concat [constr1; constr2]))
	| MapT (x, y) ->
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env y in
		(Arrow_t(t2, match t1 with Arrow_t(l, m) -> Tree_t m), (List.concat [constr1; constr2])) 
	| Filter (x, y) ->
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env y in
		(Arrow_t(t2, match t1 with Arrow_t(l, m) -> List_t m), (List.concat [constr1; constr2]))
	| FoldL (x, y, z) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env z in
		(Arrow_t(t2, match t1 with Arrow_t(l, Arrow_t (m, n)) -> n), (List.concat [constr1; constr2]))
	| FoldR (x, y, z) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env z in
		(Arrow_t(t2, match t1 with Arrow_t(l, Arrow_t (m, n)) -> n), (List.concat [constr1; constr2]))
	| FoldT (x, y, z) -> 
		let (t1, constr1) = constraints env x in
		let (t2, constr2) = constraints env z in
		(Arrow_t(t2, match t1 with Arrow_t(l, Arrow_t (m, n)) -> n), (List.concat [constr1; constr2]))
	| _ -> failwith "error"

let subst_in_typ x y z =
	let rec f z = match z with
	| Arrow_t (l,m) -> Arrow_t((f l),(f m))
	| Int_t -> Int_t
	| Bool_t -> Bool_t
	| Empty_t -> Empty_t
	| List_t l -> List_t(l)
	| Tree_t l -> Tree_t(l)
	| Pair_t (l,m) -> Pair_t((f l),(f m))
	| Var_t l -> if l = x then y else Var_t(l)
in f z


let subst_in_constr x y constr =
List.map ~f:(fun (f,g) -> (subst_in_typ x y f, subst_in_typ x y g)) constr

let occurs_in x y =
let rec f y = match y with
	| Arrow_t (l,m) -> (f l || f m)
	| Int_t -> false
	| Bool_t -> false
	| Empty_t -> false
	| List_t l -> f l
	| Tree_t l -> f l
	| Pair_t (l,m) -> (f l || f m)
	| Var_t l -> (l = x)
in f y


let rec unify constrs =
	let (++) = List.append in
	let rec f constrs = match constrs with
	| [] -> []
	| (tx, Var_t(ty)) :: xs ->
		if tx = Var_t(ty) then f xs else
		if occurs_in ty tx then failwith "error: circular constrs" else
		(f (subst_in_constr ty tx xs)) ++ [(Var_t(ty), tx)]
	| (Var_t(tx), ty) :: xs ->
		if ty = Var_t(tx) then f xs else
		if occurs_in tx ty then failwith "error: circular constrs" else
		(f (subst_in_constr tx ty xs)) ++ [(Var_t(tx), ty)] 
	| (Arrow_t(tx1, ty1), Arrow_t(tx2, ty2)) :: xs -> (f ((tx1, tx2) :: (ty1, ty2) :: xs))
	| (Int_t, Int_t) :: xs -> f xs
	| (Bool_t, Bool_t) :: xs -> f xs
	| (List_t tx1, List_t tx2) :: xs -> (f ((tx1, tx2) :: xs))
	| (tx1, List_t(tx2)) :: xs -> (f ((tx1, tx2) :: xs))
	| (tx1, Tree_t(tx2)) :: xs -> (f ((tx1, tx2) :: xs))
	| (Pair_t(tx1, tx2), Pair_t(ty1, ty2)) :: xs -> (f ((tx1, tx2) :: (ty1, ty2) :: xs))
	| (tx1, Empty_t) :: xs -> f xs
	| (Empty_t, tx1) :: xs -> f xs
	| (tx, ty) :: xs -> failwith "Error: mistyped expression" 
in f constrs


let infer env t = 
	let constrs = constraints env t in
	match constrs with (x,y) -> unify y; x
