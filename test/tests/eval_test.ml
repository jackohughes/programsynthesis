open Core.Std
open Ast
open Printf
open Eval

let eval_success = ref 0
let eval_total = ref 0

let test_set =

	[

	([	("(let x = (fun y -> + (5, y)) in (x 5))", "10");
		("(let x = (fun y -> not (y)) in (x true))", "false");
		("(let x = (fun y -> map (fun z -> * (z, 2)) y) in (x [1,2,3,4]))", "[2,4,6,8]");
		("(let x = (fun y ->  value (y)) in (x ((1001, (), ()))))", "1001"); ], "let");
	
	([	("((fun y -> + (5, y)) 5)", "10");
		("((fun y -> not (y)) true)", "false");
		("((fun y -> map (fun z -> * (z, 2)) y) [1,2,3,4])", "[2,4,6,8]");
	 	("((fun y -> value (y)) (1001, (), ()))", "1001");], "app");

	([	("(+ (1, 2, 3, 4))", "10");
		("(/ (10, 2))", "5");
		("(- (100, 50))", "50");
		("(% (10, 3))", "1");
		("(== (2, 2))", "true");
		("(!= (2, 4))", "true");
		("(!=. (true, false))", "true");
		("(==. (true, true))", "true");
		("(<= (1, 3))", "false");
		("(>= (3, 1))", "false");
		("(> (1, 3))", "true");
		("(< (3, 1))", "true");
		("(== (2, 4))", "false");
		("(!= (2, 2))", "false");
		("(!=. (true, true))", "false");
		("(==. (true, false))", "false");
		("(<= (3, 1))", "true"); 
		("(>= (1, 3))", "true");
		("(> (3, 1))", "false");
		("(< (1, 3))", "false"); 
		("(fst ((1, 2)))", "1");
		("(snd ((1, 2)))", "2");
		("(index ([1,2,3,4], 1))", "2");
		("(index ([[1],[2],[3],[4]], 1))", "[2]");], "op");

	([	("(if true then true else false)", "true");
		("(if (== ((+ (4, 2)), 6)) then 1 else 2)", "1");
		("(if (> (1, 2)) then false else true)", "false");], "if");

	([	("((+ (1,2,3)), (- (10, 5)))","(6,5)");
		("(((fun x -> * (x, 2)) 5),((fun x -> not (x)) false))","(10,true)");], "pair");

	([	("(map (fun x -> * (x, x)) [1,2,3,4])", "[1,4,9,16]");
		("(map (fun x -> not (x)) [true, false, true])", "[false, true, false]");
		("(mapt (fun x -> * (x, x)) (1, (2, (), ()), (3, (), ())))", "(1, (4, (), ()), (9, (), ()))");
		("(mapt (fun x -> not (x)) (true, (true, (false, (), ()), ()), ()))", "(false, (false, (true, (), ()), ()), ())");], "map");

	([	("(filt (fun x -> ==. (x, true)) [true, false, false, true])", "[true, true]");
		("(filt (fun x -> != (0, % (x, 3))) [3,6,9])", "[]");
		("(filt (fun x -> < (x, 1)) [1,2,3,4])", "[2,3,4]");
		("(filt (fun x -> == (x, 0)) [1,0,3,0])", "[0,0]");], "filter");

	([	("(foldr (fun x -> fun acc -> + (x, acc)) 0 [1, 2, 3, 4, 5])", "15");], "fold");

	]

let test_set_incorrect = [

	([	("+ (1, true)" , ""); 
		("index ([1, 2, 3]. 5)", "");
		("fst ([1,2,3])", "");
		("<= (true, false)", "");
		("== (true, false)", "");
		("==. (1, 2, 3)", "");
		("map (fun x -> + (x, x)) (1, 2)", "");
		("mapt (fun x -> x) [1, 2, 3, 4]", "");], "failures");

]

let eval_test () = 
	List.iter ~f:(fun (examples, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
			incr eval_total;
			let expected = Utils.lex_parse_expr y in
			let result = try eval (Env.empty()) (Utils.lex_parse_expr x) with _ ->  Utils.lex_parse_expr x in
			match result = expected with 
				| true -> incr eval_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected: " ^ (Out.expr_to_string expected) ^ "\n\tactual result: " ^  (Out.expr_to_string result) ^ "\n\n") 
				| _ -> print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected: " ^ (Out.expr_to_string expected) ^ "\n\tactual result: " ^  (Out.expr_to_string result)  ^  "\n\n")
			) examples ) test_set;
	List.iter ~f:(fun (examples, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
				incr eval_total;
				try eval (Env.empty()) (Utils.lex_parse_expr x); print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ "error" ^ "\n\tactual result: " ^ (Out.expr_to_string (eval (Env.empty()) (Utils.lex_parse_expr x))) ^  "\n\n") 
				with _ -> incr eval_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ "error" ^ "\n\tactual result: " ^ "error" ^ "\n\n")) examples ) test_set_incorrect;










