(*
	Dijkstra shunting yard algorithm of translating arithmetical expression
	into polish notation
*)

type operator = Plus
	| Minus
	| Mult
	| Div;;

type token = Operator of operator
	| LBracket
	| RBracket
	| Digit of int;;

(*tranlating algorythm*)
let transformTokens = fun tokList ->
	let rec nextTok = fun newTokList polTokList stack->
		(match newTokList with
		[] -> polTokList @ stack													(*when tokens have ended just add stack to the end of result token list*)
		| h::t ->
			(match h with
			Digit h -> nextTok t (polTokList @ [Digit h]) stack						(*when next token is digit just add it to the result token list*)
			| Operator h -> nextTok t polTokList ([Operator h] @ stack)				(*when next token is operator put it onto stack*)
			| LBracket -> nextTok t polTokList ([LBracket] @ stack)					(*when next token is open bracket than put in onto stack*)
			| RBracket ->
				(match stack with													(*when next token is close bracket then look at stack*)
				[] -> failwith "err"												(*if stack is empty - error occured*)
				| stackHead::stackTail ->
					(match stackHead with											(*in other case*)
					LBracket ->
						nextTok t polTokList stackTail								(*when open bracket than take out it from stack*)
					| _ ->
						nextTok newTokList (polTokList @ [stackHead]) stackTail		(*when other operator than  add it to out list*)
					)
				)
			)
		)
	in nextTok tokList [] []
	;
;;

(*operator printing function*)
let print_operator = fun op ->
	match op with
	Plus -> print_string "+\n"
	| Minus -> print_string "-\n"
	| Mult -> print_string "*\n"
	| Div -> print_string "/\n";;

(*token printing function*)
let print_token = fun tok ->
	match tok with
	| Operator op -> print_operator op;
	| LBracket -> print_string "(\n";
	| RBracket -> print_string ")\n";
	| Digit tok ->  print_int tok; print_newline ();;

let string_of_operator = fun str ->
	match str with
	"+" -> Plus
	| "-" -> Minus
	| "*" -> Mult
	| "/" -> Div
	| _ -> failwith "wrong operator";;

(*translating string into token*)
let	string_of_token = fun str ->
	match str with
	"+" | "-" | "*" | "/" -> Operator (string_of_operator str)
	| "(" -> LBracket
	| ")" -> RBracket
	| _ -> Digit (int_of_string str);;

(*reading line and translating it into token list*)
let line = read_line()
in let stringList = Str.split (Str.regexp "[ \t]+") line
in let tokenList = List.map string_of_token stringList
in List.map print_token (transformTokens tokenList);;
