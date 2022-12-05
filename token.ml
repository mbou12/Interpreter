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
	val keywords : (string * t) list
	val lookup_ident : string -> t
    val token_to_string : t -> string
end = struct
	type t = 
		| ILLEGAL 
		| EOF 
		| IDENT of string (* identifiers + literals *) 
		| INT of string (* NOT SURE WHY HOLDS STRING ? *)
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

	let lookup_ident (ident : string) : t = 
		let rec aux kws = begin match kws with
			| [] -> IDENT ident
			| (k::ks) -> begin match k with
				| (key, tp) -> if ident = key then tp else aux ks
				end
			end
		in
		aux keywords

    let token_to_string (token : t) : string = 
        begin match token with
            | ILLEGAL -> "ILLEGAL"
            | EOF -> "EOF"
            | IDENT id -> id
            | INT i -> i
            | ASSIGN -> "ASSIGN"
            | PLUS -> "PLUS"
            | MINUS -> "MINUS"
            | BANG -> "BANG"
            | ASTERISK -> "ASTERISK"
            | SLASH -> "SLASH"
            | LT -> "LT"
            | GT -> "GT"
            | NOT_EQ -> "NOT_EQ"
            | EQ -> "EQ"
            | COMMA -> "COMMA"
            | SEMICOLON -> "SEMICOLON"
            | LPAREN -> "LPAREN"
            | RPAREN -> "RPAREN"
            | LBRACE -> "LBRACE"
            | RBRACE -> "RBRACE"
            | FUNCTION -> "FUNCTION" 
            | LET -> "LET"
            | TRUE -> "TRUE"
            | FALSE -> "FALSE"
            | IF -> "IF"
            | ELSE -> "ELSE"
            | RETURN -> "RETURN"
        end
end

