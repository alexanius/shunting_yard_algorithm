type operator = Plus
	| Minus
	| Mult
	| Div;;

type token = Operator of operator
	| LBracket
	| RBracket
	| Digit of int;;

let print_operator = fun op ->
	match op with
	Plus -> print_string "+\n"
	| Minus -> print_string "-\n"
	| Mult -> print_string "*\n"
	| Div -> print_string "/\n"

let print_token = function tok ->
	match tok with
	| Operator op -> print_operator op;
	| LBracket -> print_string "(\n";
	| RBracket -> print_string ")\n";
	| Digit tok ->  print_int tok; print_newline ();;

(*let print_list = fun l -> List.map print_token l; print_string "====\n";;*)


let transformTokens = fun tokList ->
	let rec nextTok = fun newTokList polTokList stack->
		(match newTokList with
		[] -> polTokList @ stack												(*when tokens have ended just add stack to the end of result token list*)
		| h::t ->
			(match h with
			Digit h -> nextTok t (polTokList @ [Digit h]) stack				(*when next token is digit just add it to the result token list*)
			| Operator h -> nextTok t polTokList ([Operator h] @ stack)		(*when next token is operator put it onto stack*)
			| LBracket -> nextTok t polTokList ([LBracket] @ stack)			(*when next token is open bracket than put in onto stack*)
			| RBracket ->
(*			print_list stack;*)
				(match stack with										(*when next token is close bracket then look at stack*)
				[] -> failwith "err"											(*if stack is empty - error occured*)
				| stackHead::stackTail ->
					(match stackHead with					(*in other case*)
					LBracket ->
(*						print_token stackHead;
						print_string "(((\n";*)
						nextTok t polTokList stackTail					(*when open bracket than take out it from stack*)
					| _ ->
(*						print_token stackHead;
						print_string "---\n";*)
						nextTok newTokList (polTokList @ [stackHead]) stackTail		(*when other operator than  add it to out list*)
					)
				)
			)
		)
	in nextTok tokList [] []
	;
;;

let a = transformTokens [Digit 1; Operator Plus; LBracket; Digit 2; Operator Minus; Digit 3; RBracket]
(*let a = transformTokens [LBracket; Digit 1; Operator Plus; Digit 2; RBracket; Operator Mult; Digit 4; Operator Minus; Digit 3]*)
in print_string "\n\n== result ===\n\n"; List.map print_token a;;