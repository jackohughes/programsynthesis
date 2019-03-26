#!/bin/bash

cd src;
while getopts hc-: arg; do
  case $arg in
    h )   echo "";
          echo "         OPTIONS: ";
          echo "         -c : Recompile the program with ocamlopt (ocaml's optimising compiler)";
          echo "         -h : Show help menu";
          echo "         -test : Run the automated unit test suite and store the results in ./test/tests/results.txt";
          echo "         -benchmark : Run the benchmark suite and store the results in ./test/benchmark/results.txt";
          echo "";
          exit 0;;
    c )   echo "re-compiling..." ;
          rm -rf rm -f *.cm[iox] *~ *.out *.o;
          ocamlfind ocamlopt -linkpkg -thread -package core -package core_extended -package str -w -A ast.ml configure.ml eval.ml infer.ml out.ml learn.ml gen.ml;
          ocamllex -q lexer.mll;
          ocamlyacc parser.mly;
          ocamlfind ocamlopt -w -A parser.mli;
          ocamlfind ocamlopt -w -A lexer.ml;
          ocamlfind ocamlopt -w -A parser.ml; 
          ocamlfind ocamlopt -linkpkg -thread -package core -package core_extended -package str -w -A ast.ml eval.ml configure.ml infer.ml out.ml learn.ml lexer.ml parser.ml gen.ml utils.ml eval_test.ml cost_test.ml learn_test.ml infer_test.ml enum_test.ml;
          ocamlfind ocamlopt -linkpkg -thread -package core -package core_extended -package str -o ./out/test -w -A ast.ml eval.ml configure.ml infer.ml out.ml learn.ml lexer.ml parser.ml gen.ml utils.ml eval_test.ml cost_test.ml learn_test.ml infer_test.ml enum_test.ml test.ml;
          ocamlfind ocamlopt -linkpkg -thread -package core -package core_extended -package str -o ./out/benchmark -w -A ast.ml eval.ml configure.ml infer.ml out.ml learn.ml lexer.ml parser.ml gen.ml utils.ml benchmark.ml;
          ocamlfind ocamlopt -linkpkg -thread -package core -package core_extended -package str -o ./out/synth -w -A ast.ml eval.ml configure.ml infer.ml out.ml learn.ml lexer.ml parser.ml gen.ml utils.ml main.ml;;
    
    - )  LONG_OPTARG="${OPTARG#*=}"
         case $OPTARG in
          test       )  echo "running tests..."; 
                        ./out/test > ../test/tests/results.txt
                        cat ../test/tests/results.txt;
                        exit 0;;
          benchmark  )  echo "benchmarking...";
                        ./out/benchmark > ../test/benchmark/results.txt;
                        cat ../test/benchmark/results.txt;
                        exit 0;;
          esac ;;
  esac
done
echo "synthesising...";
./out/synth "../examples.txt"
exit 0;
