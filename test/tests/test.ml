open Core.Std
open Infer_test
open Eval_test
open Learn_test 
open Enum_test
open Cost_test

let () = 

	print_string "##########      RUNNING INFERENCE TESTS      ##########\n";
	infer_test ();
	print_string "##########     INFERENCE TESTS COMPLETE      ##########\n"; 
	print_string "##########     RUNNING EVALUATION TESTS      ##########\n"; 
	eval_test ();
	print_string "##########    EVALUATION TESTS COMPLETE      ##########\n";
	print_string "##########       RUNNING COST TESTS          ##########\n";
	cost_test ();
	print_string "##########       COST TESTS COMPLETE         ##########\n";
	print_string "##########    RUNNING ENUMERATION TESTS      ##########\n";
	enum_test ();
	print_string "##########    ENUMERATION TESTS COMPLETE     ##########\n";
	print_string "##########      RUNNING LEARNING TESTS       ##########\n";
	learn_test ();
	print_string "##########      LEARNING TESTS COMPLETE      ##########\n";
	print_string "##########         TESTING COMPLETE!         ##########\n";
	
	print_string ("INFER TEST: " ^ (string_of_int !infer_success) ^ "/" ^ (string_of_int !infer_total) ^ " TESTS PASSED\n"); 
	print_string ("EVAL TEST:  " ^ (string_of_int !eval_success) ^ "/" ^ (string_of_int !eval_total) ^ " TESTS PASSED\n");
	print_string ("COST TEST:  " ^ (string_of_int !cost_success) ^ "/" ^ (string_of_int !cost_total) ^ " TESTS PASSED\n"); 
	print_string ("ENUM TEST:  " ^ (string_of_int !enum_success) ^ "/" ^ (string_of_int !enum_total) ^ " TESTS PASSED\n");
	print_string ("LEARN TEST: " ^ (string_of_int !learn_success) ^ "/" ^ (string_of_int !learn_total) ^ " TESTS PASSED\n");
