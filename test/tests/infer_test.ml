open Core.Std
open Ast
open Out

let infer_success = ref 0
let infer_total = ref 0


let test_set = [

	([	("1", 																		Int_t);
		("true", 																	Bool_t)],									"constants");

	([	("[1, 2, 3, 4]", 															List_t(Int_t));
		("[true, false]", 															List_t(Bool_t));
		("[[1,2,3], [4,5,6], [7,8,9]]",												List_t(List_t(Int_t)));
		("[(1, (), ())]", 															List_t(Tree_t(Int_t)));
		("[(1, 2), (3, 4), (2, 1)]", 												List_t(Pair_t(Int_t, Int_t)));
		("[]"), 																	Empty_t],							"lists");

	([	("([1,2,3], (), ())", 														Tree_t(List_t(Int_t)));
		("(1, (), (2, (), ()))", 													Tree_t(Int_t));
		("((1, (), ()), (), ())",													Tree_t(Tree_t(Int_t)))],					"trees");

	([	("+ (1, 2, 3)", 															Int_t);
		("not (false)", 															Bool_t);],									"operations");

	([	("(fun x -> + (3, (hd (x))))" , 											Arrow_t(Var_t "T1", Int_t));
		("(fun x -> x)", 															Arrow_t(Var_t("T1"), Var_t("T1")));
		("(fun x -> + (x, 3))", 													Arrow_t(Var_t("T1"), Int_t));
		("(fun x -> + (fst (x), snd (x)))",											Arrow_t(Var_t("T1"), Int_t));],	"lambdas");

	([	("let x = (fun y -> + (y, y)) in (x 3)", 									Var_t("T1"));
		("let x = (fun y -> [y]) in (x 3)", 										Var_t("T1"));
		("let x = (fun y -> + (y, y)) in true", 									Bool_t);], 									"lets/apps");

	([	("map (fun x -> + (1,2,3)) [1,2,3]", 										Arrow_t (List_t (Int_t), List_t(Int_t)));
		("mapt (fun x -> + (1,2,3)) (1, (), ())", 									Arrow_t (Tree_t(Int_t), Tree_t(Int_t)));
		("foldl (fun acc -> (fun x -> + (acc, x))) 0 [1,2,3]", 						Arrow_t (List_t(Int_t), Int_t));
		("foldr (fun acc -> (fun x -> + (acc, x))) 0 [1,2,3]", 						Arrow_t (List_t(Int_t), Int_t));
		("filt (fun x -> ==. (x, true)) [true, false, true]",						Arrow_t (List_t (Bool_t), List_t (Bool_t)));],"function constants");

	([	("if true then true else false", 											Bool_t);
		("if true then [1,2,3] else [4,5,6]", 										List_t (Int_t));],			"ifs");

	([	("(1, true)", 																Pair_t(Int_t, Bool_t));
		("(true, ==. (true, true))", 												Pair_t(Bool_t, Bool_t));
		("(1, [(1, (), ())])", 														Pair_t(Int_t, List_t(Tree_t(Int_t))));],	"pairs");

]


let test_set_incorrect = [
	
	([	("+ (1, true)", 						Empty_t);
		("== (true, 1)",						Empty_t);
		("if truen then 1 else false",			Empty_t);
		("[1, 2, 3, 4, 5, true]",				Empty_t);
		("(1, (2, (), ()), (false, (), ()))",	Empty_t);], "failures");
	
]

let rec compare x y = match (x, y) with
	| Var_t x, Var_t y -> true
	| Int_t, Int_t -> true
	| Bool_t, Bool_t -> true
	| Empty_t, Empty_t -> true 
	| Pair_t (x1, y1), Pair_t (x2, y2) -> (compare x1 x2) && (compare y1 y2)
	| Arrow_t (x1, y1), Arrow_t (x2, y2) -> (compare x1 x2) && (compare y1 y2) 
	| List_t x1, List_t x2 -> compare x1 x2
	| Tree_t x1, Tree_t x2 -> compare x1 x2
	| _ -> false 

let infer_test () = 
	List.iter ~f:(fun (examples, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
				incr infer_total;
				let result = Infer.infer (Env.empty()) (Utils.lex_parse_expr x) in
				match compare result y with 
				| true -> incr infer_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ (typ_to_string y) ^ "\n\tactual result: " ^ (typ_to_string result) ^ "\n\n") 
				| false -> print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ (typ_to_string y) ^ "\n\tactual result: " ^ (typ_to_string result) ^  "\n\n")
			) examples ) test_set;
	List.iter ~f:(fun (examples, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
				incr infer_total;
				try Infer.infer (Env.empty()) (Utils.lex_parse_expr x); print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ "error" ^ "\n\tactual result: " ^ (typ_to_string (Infer.infer (Env.empty()) (Utils.lex_parse_expr x))) ^  "\n\n") 
				with _ -> incr infer_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ "error" ^ "\n\tactual result: " ^ "error" ^ "\n\n")) examples ) test_set_incorrect;




