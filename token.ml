(* ONE DAY THIS WILL WORK
module Keyword_map = Map.Make (String) 

let keywords = Keyword_map.singleton "fn" FUNCTION
	|> Keyword_map.add "let" LET
	|> Keyword_map.add "true" TRUE
	|> Keyword_map.add "false" FALSE
	|> Keyword_map.add "if" IF
	|> Keyword_map.add "else" ELSE
	|> Keyword_map.add "return" RETURN
		
let lookup_ident (ident : string)  = 
	match (Keyword_map.find_opt ident keywords) with
		| None -> ILLEGAL
		| Some t -> t
*)

module Token : sig
	type t
	val keywords : (string * t) list
	val lookup_ident : string -> t option
end = struct
	type t = 
		| ILLEGAL 
		| EOF 
		| IDENT of string (* identifiers + literals *)
		| INT of string
		| ASSIGN (* operators *)
		| PLUS
		| MINUS
		| BANG
		| ASTERISK
		| SLASH
		| LT
		| GT
		| NOT_EQ
		| EQ
		| COMMA (* delimiters *)
		| SEMICOLON
		| LPAREN
		| RPAREN
		| LBRACE
		| RBRACE
		| FUNCTION (* keywords *)
		| LET
		| TRUE
		| FALSE
		| IF
		| ELSE
		| RETURN

	let keywords = [
		("fn", FUNCTION);
		("let", LET);
		("true", TRUE);
		("false", FALSE);
		("if", IF);
		("else", ELSE);
		("return", RETURN);
	]

	let lookup_ident (ident : string) : t option = 
		let rec aux kws = begin match kws with
			| [] -> None
			| (k::ks) -> begin match k with
				| (key, tp) -> if ident = key then Some tp else aux ks
				end
			end
		in
		aux keywords
end

