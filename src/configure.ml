open Core.Std

type conf =  {
	
	assoc : bool;
	commutativity : bool;
	cache : bool;
	learning : bool;
	libraries : bool;
	laziness : bool;
	limit : int;
	exclude : string list;

}


let default disable : conf = 
	let (asc, comm, che, learn) = disable in
	{
		assoc = asc;
		commutativity = comm;
		cache = che;
		learning = learn;
		libraries = false;
		laziness = false;
		limit = 600;
		exclude = [];

	}


let configure file = 
	let r = In_channel.read_lines file in
	let split = Str.split (Str.regexp "=") in
	let last = List.last_exn r in
	let excls = match split last with [t;d] -> Str.split (Str.regexp ",") d in
	let bool_params l = List.map ~f:(fun x -> match split x with [t;d] -> bool_of_string d ) l in
	let params = match r with 
		| a :: b :: c :: d :: e :: f :: _ -> bool_params [a;b;c;d;e;f] in
	let c : conf = {
		assoc 		  = (List.nth_exn params 0); 
		commutativity = (List.nth_exn params 1); 
		cache 		  = (List.nth_exn params 2); 
		learning 	  =	(List.nth_exn params 3); 
		libraries 	  = (List.nth_exn params 4); 
		laziness 	  = (List.nth_exn params 5); 
		limit 		  =	(match split (List.nth_exn r 6) with [t;d] -> int_of_string d);
		exclude		  = excls;
	} in c


let get_deafult_confif disable = default disable

let conf = 
	try
		configure "../config.txt"
	with
	 _ -> default (true, true, true, true)

