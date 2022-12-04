(* includes the file *)
open Token

module Lexer : sig
	type t
	val init : string -> t
	val update_pos : t -> t
	val read_char : t -> t
	val skip_whitespaces : t -> t
	val peek_char : t -> char
	val is_digit : char -> bool
	val read_number : t -> string -> t * string
	val is_letter : char -> bool
	val read_identifier : t -> string -> t * string
	val next_token : t -> t * Token.t
end = struct
	(* Would really like to do this with references *)
	(* Ask prof about doing this *)
	(* Looked a little bit at the code from hw4 *)
	(* Started doing some interpreter book shit *)
	type t = {
		input : string;
		position : int; (* curr position in input -- points to curr char *)
		read_position : int; (* curr reading poisition in input -- after curr char *)
		ch : char; (* curr char under examination *)
	}

	let init (inp : string) : t =
		{
			input = inp;
			position = 0;
			read_position = 0;
			ch = '\x00';
		}

	let update_pos (l : t) : t =
		{
			input = l.input;
			position = l.read_position;
			read_position = (l.read_position + 1);
			ch = (String.get l.input l.read_position);
		}

	let read_char (l : t) : t = 
		if l.read_position >= (String.length l.input) then
			{l with ch = '\x00'}	
		else
			update_pos l

	(* Making a lot of copies of lexer object *)
	let rec skip_whitespaces (l : t) : t = 
		let next_char = read_char l in
		match next_char.ch with
		| ' ' -> skip_whitespaces next_char
		| _ -> next_char

	let peek_char (l : t) : char =
		if l.read_position >= (String.length l.input) then
			'\x00'
		else
			(String.get l.input l.read_position)
	
	(* Not even sure if this syntax will work *)
	let is_digit (c : char) : bool = begin match c with
		| '0' .. '9' -> true
		| _ -> false
	end

	(* Only handling integers for now *)
	let rec read_number (l : t) (cur : string) : (t * string) = 
		let next_char = read_char l in
		if (is_digit next_char.ch) then
				read_number next_char (cur ^ (Char.escaped next_char.ch))
		else
				(l, cur)

	let is_letter (c : char) : bool = begin match c with
		| 'a' .. 'z' -> true
		| 'A' .. 'Z' -> true
		| '_' -> true
		| _ -> false
	end

	let rec read_identifier (l : t) (cur : string) : (t * string) = 
		let next_char = read_char l in
		if (is_letter next_char.ch) then
				read_identifier next_char (cur ^ (Char.escaped next_char.ch))
		else
				(l, cur)

	(* Returns token based on character *)
	let next_token (l : t) : (t * Token.t) = 
		(* skipping whitespaces *)
		let skpd = skip_whitespaces l in
		begin match skpd.ch with 
		| '=' -> begin match (peek_char l) with
			| '=' -> ((update_pos l), EQ) (* double equals for equality *)
			| _ -> (skpd, ASSIGN)
			end
		| '+' -> (skpd, PLUS)
		| '-' -> (skpd, MINUS)
		| '!' -> begin match (peek_char l) with
			| '=' -> ((update_pos l), NOT_EQ)
			| _ -> (skpd, BANG)
			end
		| '*' -> (skpd, ASTERISK)
		| '/' -> (skpd, SLASH)
		| '<' -> (skpd, LT)
		| '>' -> (skpd, GT)
		| ';' -> (skpd, SEMICOLON)
		| '(' -> (skpd, LPAREN)
		| ')' -> (skpd, RPAREN)
		| '{' -> (skpd, LBRACE)
		| '}' -> (skpd, RBRACE)
		| '\x00' -> (skpd, EOF)
		| c -> 
			if (is_letter c) then 
					(* let (_, ident = read_identfier l c *)
					let ident = read_identifier l (Char.escaped c) in
					(* Ask prof about simpler pattern matching *)
					begin match ident with
					| (l2, i) -> begin match (Token.lookup_ident i) with
						| None -> (l2, ILLEGAL)
						| Some t -> (l2, t)
						end
					end
			else if (is_digit c) then
					let num = read_number l (Char.escaped c) in
					begin match num with
					| (l2, n) -> (l2, (INT n))
					end
			else
				(skpd, ILLEGAL)
		end
end

(*
type lexer = {
	input : string;
	position : int ref; (* curr position in input -- points to curr char *)
	read_position : int ref; (* curr reading poisition in input -- after curr char *)
	ch : char ref (* curr char under examination *)
}
*)
