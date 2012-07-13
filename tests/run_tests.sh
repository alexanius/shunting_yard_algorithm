#!/bin/sh

# This test compares my realisation of algorithm with those given in wikipedia

wiki_cpp="wiki_cpp"
g++ ../cpp_example_from_wiki/toPolish.cpp -o $wiki_cpp

my_ocaml="my_ocaml"
ocamlc str.cma ../ocaml_my_example/toPolish.ml -o $my_ocaml

# if `$wiki_cpp < 1 = D ( 2 - 3 * 4 + 5 , ! 6 , 7 )` <> `$my_ocaml < \'1 = D ( 2 - 3 * 4 + 5 , ! 6 , 7 )\'`
# then
# echo aaa
# fi
# 
echo bbb