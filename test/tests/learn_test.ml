open Ast
open Printf
open Learn
open Parser
open Out
open Gen
open Core.Std

let learn_success = ref 0
let learn_total = ref 0

let test_set_1 = 

	[
	(* list of list *)
	([	("[1, 2, 3, 4]", 																				[Num 1; Num 2; Num 3; Num 4]);
		("[2]", 																						[Num 2]);
		("[]", 																							[])], lst_of,							"list of list");

	(* list of tree *)
	([	("(2, (), ())", 																				[Num 2]);
		("(5, (), (2, (), (1, (), ())))", 																[Num 5; Num 2; Num 1])], lst_of,		"list of tree")
	]

let test_set_2 =

	[
	(* length preserving *)
	([	("[([1, 2, 3, 4, 5] -> [10, 2, 4, 5, 2])]", 													true);
		("[([] -> [])]", 																				true);
		("[((1, (3, (), ()), (4, (), ())) -> (2, (2, (), (4, (), ())), ()))]", 							true);
		("[((1, (), ()) -> (10, (), ()))]",																true);
		("[((1, (), (4, (), ())) -> (2, (2, (), (4, (), ())), ()))]",									false);
		("[((1, (), (1, (), ())) -> (10, (), ()))]",													false);
		("[([10, 12] -> [1, 2, 3])]", 																	false);
		("[([1, 2, 3] -> [])]", 																		false)], length_preserving,				"length preserving");

	(* output not larger *)
	([	("[([1, 2, 3] -> [1, 2, 3])]",																	true);
		("[([1, 2, 3] -> [])]",																			true);
		("[([1, 2, 3] -> [1, 2, 3, 4])]", 																false);
		("[([] -> [1])]",																				false)], output_not_larger,				"output not larger");		
		
	(* consistent mapping *)
	([	("[([2, 4, 2, 4, 2] -> [4, 8, 4, 8, 4])]",														true);
		("[([1] -> [3])]",																				true);
		("[((1, (3, (), ()), (3, (), (4, (), ()))) -> (10, (30, (), ()), (30, (), (40, (), ()))))]",	true);
		("[((1, (3, (), ()), (3, (), (4, (), ()))) -> (10, (30, (), ()), (25, (), (40, (), ()))))]",	false);
		("[((4, (1, (), ()), (4, (), ())) -> (14, (1, (), ()), (10, (), ())))]",						false);
		("[([1, 2, 1, 3, 2] -> [3, 2, 5, 3, 5])]",														false);
		("[([4, 3, 4, 3] -> [1, 2, 1, 3])]",															false)], consistent_mapping,			"consistent mapping");	

	(* order preserving *)
	([	("[([1, 2, 3, 4] -> [1, 2, 4])]",																true);
		("[([1, 2, 3, 4] -> [])]",																		true);
		("[([1, 2, 3, 4] -> [1])]",																		true);
		("[([1, 2, 3, 4] -> [4])]",																		true);
		("[([1, 2, 3, 4] -> [1, 3, 2])]",																false);
		("[([1, 3, 2, 4, 5] -> [5, 4, 3, 2])]",															false);], order_preserving,				"order preserving");	
	]


let test_set_3 = 

	[
	(* learn list map *)
	([	("([2, 4, 6, 2, 4] -> [3, 5, 7, 3, 5])",														1);
		("([1] -> [1])",																				1);
		("([1, 2, 3, 4] -> [1, 4, 9, 16])",																1);],									"list map");	

	(* learn list filter *)
	([	("([1, 2, 3, 2] -> [2, 2])",																	2);
		("([3, 5, 4] -> [5, 4])",																		2);],									"list filter");	

	(* learn list fold *)
	([	("([1, 2, 3, 2] -> [1, 2, 3, 5])",																3);
		("([1, 1, 1] -> 3)",																			3);
		("([1, 1, 2] -> [4])",																			3);
		("([0,0] -> 0)",																				3);
		("([1, 5, 2] -> [2, 5])",																		3)],									"list fold");

	(* learn tree map *)
	([	("((1, (2, (), ()), (3, (), (2, (), ()))) -> (10, (20, (), ()), (30, (), (20, (), ()))))",		1);
		("((2, (4, (), ()), (4, (), (2, (), ()))) -> (0, (2, (), ()), (2, (), (0, (), ()))))",			1);
		("((1, (), ()) -> (10, (), ()))",																1);
		("((1, (), (2, (), (2, (), ()))) -> (1, (), (3, (), (3, (), ()))))",							1);],									"tree map");	

	(* learn tree fold *)
	([	("((1, (), (2, (3, (), ()), ())) -> 1)",														3);
		("((10, (), (20, (), (13, (), ()))) -> [10, 20, 13])",											3);
		("((3, (), ()) -> [])",																			3);
		("((14, (), (1, (2, (), ()), ())) -> 1001)",													3)],									"tree fold");

	(* learn none *)
	([	("(1 -> [])",																					0);
		("(42 -> 42)",																					0);
		("((1, 2) -> (2, 1))",																		0);
		("(([1, 2, 3], [3, 2, 1]) -> ([3, 2, 1], [1, 2, 3]))",											0);
		("(1 -> [1, 1, 1, 1])",																			0)],									"none");
	]	


let learn_test () = 
	List.iter ~f:(fun (examples, f, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
			incr learn_total;
			let result = f (Utils.lex_parse_expr x) in
			match result = y with 
				| true -> incr learn_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ "[" ^  String.concat (to_string_list y) ^ "]" ^ "\n\tactual result: " ^ "[" ^  String.concat (to_string_list result) ^ "]" ^ "\n\n") 
				| _ -> print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ "[" ^ String.concat (to_string_list y) ^ "]" ^ "\n\tactual result: " ^ "[" ^  String.concat (to_string_list result)  ^ "]" ^   "\n\n")
			) examples ) test_set_1;

	List.iter ~f:(fun (examples, f, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
			incr learn_total;
			let result = f (Utils.lex_parse_examples x) in
			match result = y with 
				| true -> incr learn_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ (string_of_bool y) ^ "\n\tactual result: " ^ (string_of_bool result) ^ "\n\n") 
				| _ -> print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ (string_of_bool y) ^ "\n\tactual result: " ^ (string_of_bool result) ^  "\n\n")
			) examples ) test_set_2;

	List.iter ~f:(fun (examples, test_name) -> 
		List.iter ~f:(fun (x,y) -> 
			incr learn_total;
			let example = Utils.lex_parse_example x in
			let open Tuple.T2 in
			let typ = Arrow_t((inf [example] (Env.empty()) get1), (inf [example] (Env.empty()) get2)) in
			let result = learn [example] typ in
			match result = y with 
				| true -> incr learn_success; print_string (test_name ^ ": PASS - " ^ x ^ "\n\texpected result: " ^ (string_of_int y) ^ "\n\tactual result: " ^ (string_of_int result) ^ "\n\n") 
				| _ -> print_string (test_name ^ ": FAIL - " ^ x ^ "\n\texpected result: " ^ (string_of_int y) ^ "\n\tactual result: " ^ (string_of_int result) ^  "\n\n")
			) examples ) test_set_3
		
