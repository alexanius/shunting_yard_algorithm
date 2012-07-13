#!/bin/sh

# This test compares my realisation of algorithm with those given in wikipedia

wiki_cpp="wiki_cpp"
g++ ../cpp_example_from_wiki/toPolish.cpp -o $wiki_cpp

my_ocaml="my_ocaml"
ocamlc str.cma ../ocaml_my_example/toPolish.ml -o $my_ocaml

function testString () {
	A=`echo "$1" | ./$wiki_cpp`
	B=`echo "$1" | ./$my_ocaml`

	stdbuf -o0 echo "Input string: " "$1"
	stdbuf -o0 echo "Pattern result: " $A
	stdbuf -o0 echo "My result: " $B
	
	if [ $A == $B ]
	then
		echo Ok
	else
		echo Error
		exit
		
	fi

}

echo test1
testString "1 + 2"

echo test2
testString "1 + 2 + 3"

echo test3
testString "1 + 2 * 3"

echo test4
testString "1 * 3 + 2"

echo test5
testString "1 * 2 + 3 * 4"

echo test6
testString "( 1 + 2 ) * 3"

echo test7
testString "( 1 * 2 ) + 3"

echo test8
testString "( 1 + 2 ) * ! 3"

echo test9
testString "( ( ( 1 + 2 ) * ! 3 ) % 4 ) / 5"

echo test10
testString "1 = D ( 2 - 3 * 4 + 5 , ! 6 , 7 )"
