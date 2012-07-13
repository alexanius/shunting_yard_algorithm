(*
	Dijkstra shunting yard algorithm of translating arithmetical expression
	into polish notation
*)

type association = Left
	| Right;;

(*base class for all operators*)
class operator (assoc:association) (priority:int) (str:string) =
	object
		val assoc_ = assoc
		val priority_ = priority
		val string_ = str

		method assoc = assoc_
		method priority = priority_
		method to_string = string_
	end;;

(*operators itself*)
(*class comma = object inherit operator Left 0 "," end;;*)
class eq = object inherit operator Right 1 "=" end;;
class plus = object inherit operator Left 2 "+" end;;
class minus = object inherit operator Left 2 "-" end;;
class mult = object inherit operator Left 3 "*" end;;
class div = object inherit operator Left 3 "/" end;;
class mod_op = object inherit operator Left 3 "%" end;;				(*because 'mod' is keyword*)
class not = object inherit operator Right 4 "!" end;;
class func name = object inherit operator Right 5 name end;;		(*when using a function you should give it name*)

type token = Operator of operator
	| LBracket
	| RBracket
	| Comma
	| Digit of int;;

(*tranlating algorythm*)
let transformTokens = fun tokList ->
	let rec nextTok = fun newTokList polTokList stack->
		(match newTokList with
		[] -> polTokList @ stack													(*when tokens have ended just add stack to the end of result token list*)
		| h::t ->
			(match h with
			Digit d -> nextTok t (polTokList @ [Digit d]) stack						(*when next token is digit just add it to the result token list*)
			| Comma ->
				(match stack with
				[] -> failwith "err"												(*if stack is empty - error occured*)
				| stackHead::stackTail ->
					(match stackHead with	
						LBracket -> nextTok t polTokList stack
						| _ -> nextTok newTokList (polTokList @ [stackHead]) stackTail		(*move other oprators from stack to result list*)
					)
				)
			| Operator op ->
				(match stack with
				stackHead::stackTail ->
					(match stackHead with															(*while operator op2 is on the stack top *)
					Operator op2 when
						( op#assoc = Left && op#priority <= op2#priority ) ||
						( op#assoc = Right && op#priority < op2#priority ) ->
						nextTok newTokList (polTokList @ [Operator op2]) stackTail		(*move other oprators from stack to result list*)
					| _ -> nextTok t polTokList ([Operator op] @ stack)								(*if no operator on stack, add crurren op to stack and carry on*)
					)
				| [] -> nextTok t polTokList ([Operator op] @ stack)				(*when next token is operator put it onto stack*)
				)
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
;;

(*token printing function*)
let print_token = fun tok ->
	match tok with
	LBracket -> print_string "(";
	| RBracket -> print_string ")";
	| Comma -> print_string ",";
	| Digit tok ->  print_int tok;
	| Operator op -> print_string op#to_string;
;;

(*translating string into token*)
let	string_of_token = fun str ->
	match str with
	"+" -> Operator (new plus);
	| "-" -> Operator (new minus);
	| "*" -> Operator (new mult);
	| "/" -> Operator (new div);
	| "%" -> Operator (new mod_op);
	| "!" -> Operator (new not);
	| "=" -> Operator (new eq);
	| "," -> Comma;
	| "(" -> LBracket;
	| ")" -> RBracket;
	| _ as name when ((String.get name 0) >= 'A') && ((String.get name 0) <= 'Z') -> Operator (new func name);
	| _ -> Digit (int_of_string str);
;;

(*reading line and translating it into token list*)
let line = read_line()
in let stringList = Str.split (Str.regexp "[ \t]+") line
in let tokenList = List.map string_of_token stringList
in List.map print_token (transformTokens tokenList);;
print_newline ();;
