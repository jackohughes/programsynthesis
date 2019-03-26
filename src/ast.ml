open Core.Std



module Env = struct
	
	type 'a t = 'a String.Map.t ref
	let empty () : 'a t = ref String.Map.empty
	let lookup env id = String.Map.find !env id
	let lookup_exn env id = match  (String.Map.find !env id) with
		| Some x -> x
		| None -> failwith "error"
	let bind env id data = ref (String.Map.add !env ~key:id ~data:data)
	let keys env = String.Map.keys !env
	let data env = String.Map.data !env
	let rmv env id = String.Map.remove id !env

end

module Tree = struct

	type 'a t =
  	| Node of 'a * 'a t * 'a t
  	| Leaf 

  	let rec map t ~f = match t with
    | Leaf -> Leaf
    | Node (n, l, r) -> Node (f n, (map l ~f:f), (map r ~f:f))

end

type var = string

type typ = 
	| Int_t
	| Bool_t
	| Var_t of string
	| Unit_t
	| Empty_t
	| Arrow_t of typ * typ
	| List_t of typ
	| Tree_t of typ
	| Pair_t of typ * typ

type op = 
	| Add
	| Sub		
	| Mul
	| Div
	| Mod
	| Eq_Int
	| Eq_Bool
	| Neq_Int
	| Neq_Bool
	| Grt
	| Geq
	| Ls
	| LsEq
	| Index
	| Branch
	| Not
	| Head
	| Tail
	| Value
	| Children
	| Fst
	| Snd
	| Append
	| Concat


type expr =
	| Num of int
	| Bool of bool
	| Var of var 
	| Let of var * expr * expr
	| Lambda of var * expr
	| App of expr * typ *  expr
	| Closure of var * expr * (typ * expr) Env.t
	| Op of op * expr list
	| If of expr * expr * expr
	| List of lst
	| Pair of expr * expr
	| Tree of expr Tree.t
	| Map of expr * expr
	| MapT of expr * expr
	| Filter of expr * expr
	| FoldL of expr * expr * expr
	| FoldR of expr * expr * expr
	| FoldT of expr * expr * expr

and lst =
	| Empty
	| Cons of expr * lst


