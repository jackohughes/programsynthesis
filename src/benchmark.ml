open Gen
open Ast
open Ast.Tree
open Printf
open Core.Std.Float
open Utils
open Core.Std

let count = ref 0
let count_asoc = ref 0
let count_comm = ref 0
let count_cache = ref 0
let count_learn = ref 0

let examples = [

("[	(0	 -> 0),
	(1	 -> 1),
	(2	 -> 4),
	(3	 -> 9)]", "square");

("[	(0	 -> 0),
	(1	 -> 1),
	(2	 -> 8),
	(3	 -> 27)]", "cube");

("[	([0,1,2,3]	 -> [0,1,4,9]),
	([] -> [])]", "map square");

("[	([0,1,2,3]	 -> [0,1,8,27]),
	([] -> [])]", "map cube");

("[	(([1, 2, 3, 4], 1)		 -> [2, 3, 4, 5]),
	(([34, 54, 0, 0], 6)	 -> [40, 60, 6, 6])]", "add"); 

("[	(([1, 2, 3, 4], 5)		 -> [1, 2, 3, 4, 5]),
	(([14, 4, -4], 6)		 -> [14, 4, -4, 6])]", "append"); 

("[	(([1, 2], [3, 4])		 -> [1, 2, 3, 4]),
	(([1], [-1])			 -> [1, -1])],
	(([1, 4], [32, 4])		 -> [1, 4, 32, 4])]", "concat");

("[	([1, 2, 3, 4, 5]		 -> [1, 2, 3, 4]), 
	([7, 1, 5, 7]			 -> [7, 1, 5]),
	([2, 1]					 -> [2])]", "droplast"); 

("[	([1, 2, 3, 4, 5]		 -> [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]),
	([3, 3, 2, 2]			 -> [3, 3, 2, 2, 3, 3, 2, 2]),
	([1]					 -> [1, 1])]", "duplicate"); 

("[	([1, 2, 3, 4, 5, 6]		 -> [2, 4, 6]),
	([4, 5, 6, 7, 8, 9]		 -> [4, 6, 8]),
	([2, 2, 4, 4, 6, 6]		 -> [2, 2, 4, 4, 6, 6])]", "evens"); 

("[	([1, 2, 3, 4]		 	 -> 4),
	([2]					 -> 2),
	([3, 4, 5]				 -> 5)]", "last"); 

("[	([1, 2, 3, 4]		 -> 4),
	([2]					 -> 1),
	([3, 4, 5]				 -> 3),
	([]						 -> 0),
	([1, 2, 3, 4, 5, 6]		 -> 6)]", "length"); 

("[	([1, 21, 3, 4]			 -> 21),
	([2]					 -> 2),
	([3, 4, 5]				 -> 5),
	([1, 2, 34, 4, 5, 6]	 -> 34)]", "max"); 

("[	(([1, 2, 3, 4], 1)		 -> true),
	(([34, 54, -4, 0], 6)	 -> false),
	(([1, 2, 32, 4], 32)	 -> true),
	(([34, 54, -4, 0], 6)	 -> false)]", "member"); 

("[	([43, 32, 21, 5, 2, -64] -> [43, 43, 43, 43, 43, 43]),
	([2, 4]					 -> [2, 2]),
	([-1, 34, 566, 43, 3]	 -> [-1, -1, -1, -1, -1]),
	([1]					 -> [1])]", "multifirst"); 

 ("[([43, 32, 6, 2, -64]	 -> [-64, -64, -64, -64, -64]),
	([2, 4]					 -> [4, 4]),
	([-1, 34, 566, 43, 3]	 -> [3, 3, 3, 3, 3]),
	([]						 -> []),
	([1]					 -> [1])]", "multilast"); 

("[	([43, 32, 21, 6, 2, -64] -> [-64, 2, 6, 21, 32, 43]),
	([2, 4]					 -> [4, 2]),
	([-1, 34, 566, 43, 3]	 -> [3, 43, 566, 34, -1]),
	([]						 -> []),
	([1]					 -> [1])]", "reverse");  

("[	([43, 32, 21, 6, 2, -64] -> [32, 21, 6, 2, -64, 43]),
	([1, 2, 3, 4, 5]		 -> [2, 3, 4, 5, 1]),
	([2, 4]					 -> [4, 2])]", "shiftl"); 


("[	([43, 32, 21, 6, 2, -64] -> [-64, 43, 32, 21, 6, 2]),
	([2, 4]					 -> [4, 2]),
	([-1, 34, 566, 43, 3]	 -> [3, -1, 34, 566, 43])]", "shiftr"); 

("[	([1, 21, 3, 4]			 -> 29),
	([2]					 -> 2),
	([3, 4, 5]				 -> 12),
	([1, 2, 34, 4, 5, 6]	 -> 52)]", "sum");

("[	((1, (1, (), ()), (1, (), ()))			 -> 3),
	((1, (), ())							 -> 1),
	((1, (1, (), ()), (1, (1, (), ()), ()))	 -> 4)]" , "count nodes"); 

("[
	((1, (), ())							 -> (2, (), ())),
	((1, (0, (), ()), ())					 -> (2, (1, (), ()), ()))]" , "increment");


("[	(((4, (1, (), ()), (3, (), ())), 1)									 -> true),
	(((2, (1, (), (2, (), (3, (), ()))), (33, (1, (), ()), ())), 23)	 -> false),
	(((4, (), ()), 4)													 -> true),
	(((4, (1, (), ()), (3, (), ())), 1)									 -> true),
	(((4, (1, (), ()), (3, (), ())), 32)								 -> false)]" , "membert"); 

("[	((3, (2, (), (11, (), ())), (10, (), ())) 	 -> 26),
	((-1, (), ())								 -> -1),
	((4, (3, (), ()), (32, (), ()))			 	 -> 39)]" , "sumt");

("[	((([2, 3, 4], ([1, 2, 3, 4, 5], (), ()), ()), 1) 										 -> ([2, 3, 4, 1], ([1, 2, 3, 4, 5, 1], (), ()), ())),
	((([2], (),  ([3, 4, 5], ([10, 11, 12], (), ()), ([13, 14], (), ()))), 4)				 -> ([2, 4], (), ([3, 4, 5, 4], ([10, 11, 12, 4], (), ()), ([13, 14, 4], (), ())))),
	((([1, 2, 3], (), ()), 1001)														 	 -> ([1, 2, 3, 1001], (), ()))]" , "appendt");

("[	(([1, 2, 3], (), ([4, 5, 6], (), ()))													-> [1, 2, 3, 4, 5, 6]),
	(([2, 4], (), ([6, 8], (), ()))															-> [2, 4, 6, 8]),
	(([1], (), ())																			-> [1])]" , "flattenl"); 

("[	([[4, 6, 1], [1, -2, 3], [1], [5, 2, 0], [10, 2, 6], [3, 5]]					 		-> [[5, 7, 2], [2, -1, 4], [2], [6, 3, 1], [11, 3, 7], [4, 6]]),
	([[1, 2, 3, 4], [3, -1], [100, 233, -1], [3]]											-> [[2, 3, 4, 5], [4, 0], [101, 234, 0], [4]]),
	([[2, 3, -2, 4]]																		-> [[3, 4, -1, 5]])]" , "incrs");

("[	([[4, 6, 1], [1, -2, 3], [1], [5, 2, 0], [10, 2, 6], [3, 5]]					 		-> [4, 6, 1, 1, -2, 3, 1, 5, 2, 0, 10, 2, 6, 3, 5]),
	([[1, 2, 3, 4], [3, -1], [100, 233, -1], [1, 2, 3, 4, 3, -1]]					 		-> [1, 2, 3, 4, 3, -1, 100, 233, -1, 1, 2, 3, 4, 3, -1]),
	([[2, 3, -2, 4]]																		-> [2, 3, -2, 4]) ]" , "join"); 

("	[(([2, 4, 6, 8], (), ([1, 1, 1], (), ([10, 10, 16], (), ([3, 2, 0], (), ()))))			-> (20, (), (3, (), (36, (), (5, (), ()))))),
	(([5, 5, 5], (), ())																	-> (15, (), ()))]" , "sumnodes"); 

]


let limit = Configure.conf.limit

let times = ref ""
let all_opt_times = ref []
let assoc_times = ref []
let comm_times = ref []
let cache_times = ref []
let learn_times = ref []

let i = ref 0

let headers = "Test, All Optimsiations, No Associativity, No Commutativity, No Cache, No Learning\n"

let () =
	print_string "\nALL OPTIMISATIONS\n";
	List.iter ~f:(fun (x,y) -> match (timeout (lex_parse_examples x) "default" limit `Time_out) with 
		| `Time_out -> all_opt_times := -1.0 :: !all_opt_times; incr count; print_int !count; print_string ": "; printf "%s - Timeout \n" y;
		| `Result res ->  match res with (pr, total, suc, time) -> all_opt_times := time :: !all_opt_times; incr count; print_int !count; print_string ": "; printf "%s - Time Taken: %f \n" y time;) examples;
	print_string "\nNO ASSOCIATIVITY\n";
	List.iter ~f:(fun (x,y) -> match (timeout (lex_parse_examples x) "assoc" limit `Time_out) with 
		| `Time_out -> assoc_times := -1.0 :: !assoc_times; incr count_asoc; print_int !count_asoc; print_string ": "; printf "%s - Timeout \n" y;
		| `Result res ->  match res with (pr, total, suc, time) -> assoc_times := time :: !assoc_times; incr count_asoc; print_int !count_asoc; print_string ": "; printf "%s - Time Taken: %f\n" y time;) examples;
	print_string "\nNO COMMUTATIVITY\n";
	List.iter ~f:(fun (x,y) -> match (timeout (lex_parse_examples x) "comm" limit `Time_out) with
		| `Time_out -> comm_times := -1.0 :: !comm_times; incr count_comm; print_int !count_comm; print_string ": "; printf "%s - Timeout \n" y;
		| `Result res ->  match res with (pr, total, suc, time) -> comm_times := time :: !comm_times; incr count_comm; print_int !count_comm; print_string ": "; printf "%s - Time Taken: %f\n" y time;) examples;
	print_string "\nNO CACHE\n";
	List.iter ~f:(fun (x,y) -> match (timeout (lex_parse_examples x) "cache" limit `Time_out) with
		| `Time_out -> cache_times := -1.0 :: !cache_times; incr count_cache; print_int !count_cache; print_string ": "; printf "%s - Timeout \n" y;
		| `Result res ->  match res with (pr, total, suc, time) -> cache_times := time :: !cache_times ; incr count_cache; print_int !count_cache; print_string ": "; printf "%s - Time Taken: %f\n" y time;) examples;
	print_string "\nNO LEARNING\n";
	List.iter ~f:(fun (x,y) -> match (timeout (lex_parse_examples x) "learn" limit `Time_out) with 
		| `Time_out -> learn_times := -1.0 :: !learn_times; incr count_learn; print_int !count_learn; print_string ": "; printf "%s - Timeout \n" y;
		| `Result res ->  match res with (pr, total, suc, time) -> learn_times := time :: !learn_times; incr count_learn; print_int !count_learn; print_string ": "; printf "%s - Time Taken: %f\n" y time;) examples; 



	List.iter ~f:(fun (x,y) -> times := !times ^ y ^ ", " ^ (to_string_hum ~decimals:3  (List.nth_exn !all_opt_times !i)) ^ ", " ^ (to_string_hum ~decimals:3  (List.nth_exn !assoc_times !i)) ^ ", " ^
								(to_string_hum ~decimals:3 (List.nth_exn !comm_times !i)) ^ ", " ^ (to_string_hum ~decimals:3  (List.nth_exn !cache_times !i)) ^ ", " ^ 
								(to_string_hum ~decimals:3 (List.nth_exn !learn_times !i)) ^ "\n"; incr i) examples;
	Out_channel.write_all "../test/benchmark/data.csv" ~data:(headers ^ !times);


