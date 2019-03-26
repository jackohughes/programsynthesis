open Core.Std
open Out
open Gen
open Ast


let enum_success = ref 0
let enum_total = ref 0

let test_set_enum_correct_weights = [

	(0, Int_t);

	(1, Int_t);

	(2, Int_t);

	(3, Int_t);

	(4, Int_t);

	(5, Int_t);

	(0, Bool_t);

	(1, Bool_t);

	(2, Bool_t);

	(3, Bool_t);

	(4, Bool_t);

	(5, Bool_t);

	(0, List_t (Int_t));

	(1, List_t (Int_t));

	(2, List_t (Int_t));

	(3, List_t (Int_t));

	(4, List_t (Int_t));

	(5, List_t (Int_t));

	(0, Tree_t (Int_t));

	(1, Tree_t (Int_t));

	(2, Tree_t (Int_t));

	(3, Tree_t (Int_t));

	(4, Tree_t (Int_t));

	(5, Tree_t (Int_t));

	(0, Pair_t(Int_t, Int_t));

	(1, Pair_t(Int_t, Int_t));

	(2, Pair_t(Int_t, Int_t));

	(3, Pair_t(Int_t, Int_t));

	(4, Pair_t(Int_t, Int_t));

	(5, Pair_t(Int_t, Int_t));
	
]



		
let test_enum_correct_weights () = 

		List.iter ~f:(fun (x, typ) -> 
			incr enum_total;
			let result = enum x typ (Env.empty()) (State.init()) ~lrn_state:0 ~ch:true ~assc:true ~comm:true in
			let rec check y = match y with 
				| l :: ls -> ((cost l) = x) && check ls 
				| [] -> true in
			match check result with
				| true -> incr enum_success; print_string ("enumerate programs of correct weight" ^ ": PASS - weight: " ^ (string_of_int x) ^ " -  type: " ^ (typ_to_string typ) ^ "\n\n") 
				| false -> print_string ("enumerate programs of correct weight" ^ ": FAIL - wieght: " ^ (string_of_int x) ^ " - type: " ^ (typ_to_string typ) ^ "\n\n")) test_set_enum_correct_weights


let enum_test () =  test_enum_correct_weights ();



