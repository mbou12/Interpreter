(* includes the file *)
open Token

module Lexer : sig
	type t
	val init : string -> t
    val get_char : t -> char
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
			read_position = 1;
            ch = String.get inp 0;
		}

    let get_char (l : t ) : char = l.ch

	let update_pos (l : t) : t =
		{
			input = l.input;
			position = l.read_position;
			read_position = (l.read_position + 1);
            ch = String.get l.input l.read_position;
		}

	let read_char (l : t) : t = 
		if l.read_position >= (String.length l.input) then
			{l with ch = '~'}	
		else
			update_pos l

	(* Making a lot of copies of lexer object *)
	let rec skip_whitespaces (l : t) : t = 
        begin match l.ch with
            | ' ' -> skip_whitespaces (read_char l)
            | '\t' -> skip_whitespaces (read_char l) 
            | '\n' -> skip_whitespaces (read_char l)
            | '\r' -> skip_whitespaces (read_char l) 
            | _ -> l
        end

	let peek_char (l : t) : char =
		if l.read_position >= (String.length l.input) then
			'~'
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
        (* reading the next character ahead of time -- this updates the pos *)
        let skpd_next = read_char skpd in
		begin match skpd.ch with 
            | '=' -> 
                begin match skpd_next.ch with
                    | '=' -> (read_char skpd_next, EQ) (* double equals for equality *)
                    | _ -> (skpd_next, ASSIGN)
                end
            | '+' -> (skpd_next, PLUS)
            | '-' -> (skpd_next, MINUS)
            | '!' -> 
                begin match skpd_next.ch with
                    | '=' -> (read_char skpd_next, NOT_EQ)
                    | _ -> (skpd_next, BANG)
                end
            | '*' -> (skpd_next, ASTERISK)
            | '/' -> (skpd_next, SLASH)
            | '<' -> (skpd_next, LT)
            | '>' -> (skpd_next, GT)
            | ';' -> (skpd_next, SEMICOLON)
            | ',' -> (skpd_next, COMMA)
            | '(' -> (skpd_next, LPAREN)
            | ')' -> (skpd_next, RPAREN)
            | '{' -> (skpd_next, LBRACE)
            | '}' -> (skpd_next, RBRACE)
            | '~' -> (skpd_next, EOF)
            | c -> 
                if (is_letter c) then 
                    let ident = read_identifier skpd (Char.escaped c) in
                    let (l2, i) = ident in
                    (read_char l2, Token.lookup_ident i)
                else if (is_digit c) then
                    let num = read_number skpd (Char.escaped c) in
                    let (l2, n) = num in
                    (read_char l2, INT n)
                else
                    (skpd_next, ILLEGAL)
		end
end

