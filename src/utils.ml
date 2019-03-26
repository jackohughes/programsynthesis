open Ast
open Event
open Thread
open Gen
open Printf
open Core


exception Timeout

let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)

let timeout arg1 arg2 time default_value =
   let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
   let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior 
   in ignore (Unix.alarm time) ;
      try  let res = synthesise arg1 ~conf:arg2 in reset_sigalrm () ; `Result res with exc -> reset_sigalrm (); if exc=Timeout then default_value else raise exc


let lex_parse_expr input =
          try
            let lexbuf = Lexing.from_string input in
              let result = Parser.expr Lexer.token lexbuf in
              match result with
                | x -> x
                | _ -> failwith "error"
          with _ -> failwith "error"

let lex_parse_example input =
          try
              let lexbuf = Lexing.from_string input in
              let result = Parser.ex Lexer.token lexbuf in
              match result with
                | (x, y) -> (x, y)
                | _ -> failwith "error"
          with _ -> failwith "error"

let lex_parse_examples input = 
    try
              let lexbuf = Lexing.from_string input in
              let result = Parser.examples Lexer.token lexbuf in
              match result with
                | x -> x
                | _ -> failwith "error in examples - examples cannot be parsed"
          with _ -> failwith "error in examples - examples cannot be parsed"

