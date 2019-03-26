open Core.Std
open Ast

let (++) = List.append


let rec mem x = function
  | [] -> false
  | h::t -> h = x || mem x t


let rec remove x = function
  | [] -> failwith "x not in list"
  | h::t -> if h = x then t else h::(remove x t)


let rec intersect a b = match a with
	| [] -> if b = [] then [] else intersect b a
	| h::t -> if mem h b then let b' = remove h b in h::(intersect t b') else intersect t b


let lst_of l =
	let rec tree_to_list t = let open Ast.Tree in match t with
		| Node(x, l, r) -> (tree_to_list l) ++ [x] ++ (tree_to_list r)
		| Leaf -> [] in
	let rec lst_contents c = match c with
		| Cons (x, y) -> x :: lst_contents y
		| Empty -> [] in match l with List(c) -> lst_contents c | Tree(c) -> tree_to_list c


let rec length_preserving e = (match e with
	| (i,o) :: xs -> if (List.length (lst_of i)) = (List.length (lst_of o)) then length_preserving xs else false 
	| [] -> true)


let rec output_not_larger e = (match e with
	| (i,o) :: xs -> if (List.length (lst_of i)) >= (List.length (lst_of o)) then output_not_larger xs else false 
	| [] -> true )


let rec consistent_mapping e = (match e with
	| (i,o) :: xs -> 
		let rec find_dups l i v = (match l with
			| x :: xs -> if v = x then i :: (find_dups xs (i+1) v) else (find_dups xs (i+1) v)
			| [] -> []) in
		let rec compare l t = (match l with
			| x :: xs -> if match List.nth (lst_of o) x with | Some w -> w = t | None -> false then compare xs t else false
			| [] -> true) in
		let rec map_compare io = (match io with
			| (x :: xs, y :: ys) -> let m = (find_dups (lst_of i) 0 x) in if not (compare m y) then false else map_compare (xs,ys)
			| ([],[]) -> true) in if map_compare ((lst_of i),(lst_of o)) then consistent_mapping xs else false
		| [] -> true)


let rec order_preserving e = (match e with
	| (i,o) :: xs -> if (intersect (lst_of i) (lst_of o)) = (lst_of o) then order_preserving xs else false
	| [] -> true )


let learn e typ = match typ with 
	| Arrow_t (List_t x, List_t y) -> 
		let lp = (length_preserving e) in
		let cm = if lp = true then (consistent_mapping e && consistent_mapping (List.map ~f:(fun x -> Tuple.T2.swap x) e)) else false in
		let x = match (lp,  cm, (order_preserving e) , (output_not_larger e )) with
			| (true, true, _, _) -> 1 			(* Map, Fold *)
			| (_, _, true, true) -> 2 			(* Filter, Fold *)
			| _ -> 3 in x 						(* Fold *)
	| Arrow_t (List_t x, y) -> 3 				(* Fold *)
	| Arrow_t (Tree_t x, Tree_t y) -> 
		let lp = (length_preserving e) in
		let cm = if lp = true then (consistent_mapping e && consistent_mapping (List.map ~f:(fun x -> Tuple.T2.swap x) e)) else false in
		let x = match ((length_preserving e),  (consistent_mapping e && consistent_mapping (List.map ~f:(fun x -> Tuple.T2.swap x) e))) with
			| (true, true) -> 1					(* MapT *)
			| _ -> 3 in x						(* FoldT *)
	| Arrow_t (Tree_t x, y) -> 3 			(* FoldT *)
	| _ -> 0


