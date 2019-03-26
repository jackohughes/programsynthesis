open Core.Std
open Ast
open Printf

let rec typ_to_string e = let str = (match e with
	| Empty_t -> "Empty_t"
	| Int_t -> "Int_t"
	| Bool_t -> "Bool_t"
	| Var_t x -> "Var_t " ^ x
	| List_t x -> "[" ^ typ_to_string x ^ "]"
	| Tree_t x -> "(" ^ typ_to_string x ^ ")"
	| Pair_t (x, y) -> "(" ^ typ_to_string x ^ ", " ^ typ_to_string y ^ ")"
	| Arrow_t (x, y) -> "(" ^ typ_to_string x ^ " -> " ^ typ_to_string y ^ ")"
 	) in str 

let rec expr_to_string e = let str = (match e with
	| Num x -> (string_of_int x)
	| Bool b -> (string_of_bool b)
	| Var v -> v
	| Lambda (x,y) -> "(fun " ^ x ^ " -> " ^ (expr_to_string y) ^")"
	| Let (x,y,z) -> "let " ^ x ^ " = " ^ (expr_to_string y) ^ " in " ^ (expr_to_string z)
	| If (x,y,z) -> "if " ^ (expr_to_string x) ^ " then " ^ (expr_to_string y) ^ " else " ^ (expr_to_string z) 
	| App (x,t,z) -> "app (" ^ (expr_to_string x) ^ " " ^ (expr_to_string z) ^ ")"
	| Op (x,y) -> let d = (match x with
		| Add -> "+"
		| Sub -> "-"
		| Mul -> "*"
		| Div -> "/"
		| Mod -> "%"
		| Eq_Int -> "=="
		| Neq_Int -> "!="
		| Grt -> ">"
		| Geq -> ">="
		| Ls -> "<"
		| LsEq -> "<="
		| Eq_Bool -> "==."
		| Neq_Bool -> "!=."
		| Not -> "not"
		| Tail -> "tl"
		| Head -> "hd"
		| Index -> "index"
		| Branch -> "branch"
		| Children -> "children"
		| Value -> "value"
		| Fst -> "fst"
		| Snd -> "snd"
		| Append -> "apnd"
		| Concat -> "concat")
	in "(" ^ (let rec ops o = (match o with
			| x :: [] -> expr_to_string x
			| x :: xs -> expr_to_string x ^ " " ^ d ^ " " ^ ops xs) in if List.length y = 1 then d ^ " " ^ expr_to_string (List.hd_exn y) else ops y) ^ ")"
	| FoldR (x, y, z) -> "foldr (" ^ (expr_to_string x) ^ ") " ^ (expr_to_string y) ^ " " ^ (expr_to_string z)
	| FoldL (x, y, z) -> "foldl (" ^ (expr_to_string x) ^ ") " ^ (expr_to_string y) ^ " " ^ (expr_to_string z)
	| FoldT (x, y, z) -> "foldt (" ^ (expr_to_string x) ^ ") " ^ (expr_to_string y) ^ " " ^ (expr_to_string z)
	| Map (x, y) -> "map (" ^ (expr_to_string x) ^ ") " ^ (expr_to_string y) 
	| Filter (x, y) -> "filter (" ^ (expr_to_string x) ^ ") " ^ (expr_to_string y)
	| MapT (x, y) -> "mapt (" ^ (expr_to_string x) ^ ") " ^ (expr_to_string y)
	| Pair (x, y) -> "(" ^ (expr_to_string x) ^ ", " ^ (expr_to_string y) ^ ")"
	| List(x) -> let rec lst l = (match l with
		| Empty -> ""
		| Cons (x, Empty) -> (expr_to_string x)
		| Cons (x, y) -> (expr_to_string x) ^ ", " ^ (lst y)) in "[" ^ lst x ^ "]"
	| Tree (x) -> let rec tr t = (match t with
		| Tree.Leaf -> "()"
		| Tree.Node (x, y, z) -> "(" ^ (expr_to_string x) ^ ", " ^ (tr y) ^ ", " ^ (tr z) ^ ")" ) in tr x
	| _ -> failwith "error" ) in str 

let to_string_list x = List.map ~f:(fun r -> (expr_to_string r) ^ ", ") x
let to_string x = List.map ~f:(fun r -> (expr_to_string r) ^ "\n") x
let out x = Out_channel.write_all "../test/enumeration_output/output.txt" ~data:(String.concat (to_string x))