open Ast
open Event
open Thread
open Gen
open Printf
open Core.Std
open Utils


let limit = Configure.conf.limit

let read_examples x = 
  let r file = In_channel.read_all file in 
  lex_parse_examples (r x)


let () = match timeout (read_examples "../examples.txt") "default" limit `Time_out with 
   | `Time_out -> printf "Timeout after %i seconds - No solution found!\n" limit
   | `Result res -> match res with (pr, total, suc, time) -> printf "\nTarget Program: %s \n\nTime Taken: %f, Executed Successfully: %i/%i \n\n" (Out.expr_to_string pr) time suc total;

  