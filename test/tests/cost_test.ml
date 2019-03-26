open Core.Std
open Out
open Gen
open Ast


let cost_success = ref 0
let cost_total = ref 0

let test_set_weight =
	
	[

		([	("0", 																					0);
			("true", 																				0);
			("v1",																					0);
			("10001",																				0);], "cost 0");

		([	("(1, 2)",																				1);
			("(fun x -> x)",																		1);
			("[1]",																					1);
			("(1, (), ())",																			1);
			("(v1 v2)",																				1);
			("+ (1, 2)",																			1);], "cost 1");

		([	("(1, (1, 2))",																			2);
			("[1,2]",																				2);
			("(fun x -> (fun y -> y))",																2);
			("(fun x -> + (x, 2))",																	2);
			("(1, (2, (), ()), ())",																2);
			("+ (1, 3, 1)",																			2);], "cost 2"); 

		([	("[1,2,3]",																				3);
			("(fun x -> (fun y -> (fun z -> z)))",													3);
			("((fun x -> fun y -> y) 1)",															3);
			("((1,2), (3,4))",																		3);
			("(1, (2, (), ()), (3, (), ()))",														3);
			("/ (1, 2, 3, 4)",																		3);], "cost 3");

		([	("(fun x -> + (1, 2, 3, 4))",															4);
			("([1, 2], (2, 3))",																	4);
			("[[1, 2, 3]]",																			4);
			("(fun x -> (fun y -> [x,y]))",															4);
			("let x = (fun y -> + (y, 1)) in (x 1)",												4);
			("(1, (2, (3, (), ()), ()), (4, (), ()))",												4);], "cost 4");

		([	("(fun x -> (fun y -> + (x, y, 2, 3)))",												5);
			("let x = (fun y -> y) in ((x 1), (x 2))",												5);
			("[1, 2, 3, 4, 5]",																		5);
			("(fun x -> (fun y -> (fun z -> (fun a -> (fun b -> y)))))",							5);
			("if (== (1, 3, 2)) then 1 else 2",														5);
			("+ (1, 2, 3, 4, 5, 6)",																5);], "cost 5");

		([	("let x = (fun y -> * (y, 10, y)) in [(y 1), (y 2), (y 3)]",							10);
			("[[1, 2], [3, 4], [5, 6, 7]]",															10);
			("map (fun x -> + (1, % (x, 2))) [1,2,3,4,5,6]",										10);], "cost 10");

		([	("let x = (fun y -> * (y, 10, y)) in [(y 1), (y 2), (y 3), (y (+ (1, 2, (y 3))))]",		15);
			("[[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]",											15);
			("foldr (fun acc -> (fun y -> + (acc, y, 3))) 0 [1,2,3,4,5,6,7,8,9,10]",				15);], "cost 15");
	]


let cost_test () = 

		List.iter ~f:(fun (examples, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
			incr cost_total;
			let expr = Utils.lex_parse_expr x in
			let result = cost expr in
			match result = y with 
				| true -> incr cost_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ (string_of_int y) ^ "\n\tactual result: " ^ (string_of_int result) ^ "\n\n") 
				| _ -> print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ (string_of_int y) ^ "\n\tactual result: " ^ (string_of_int result) ^  "\n\n")
			) examples ) test_set_weight;
		



