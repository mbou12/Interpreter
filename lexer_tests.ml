open OUnit2
open Lexer
open Token

(*

Quick Note on OUnit2 documentation

>::: "to define a list of tests"
>:: "to name a test"
run_test_tt_main "to run the test suite you define"
assert_equal "you know what it is"

*)


(* Given a lexer, collect the tokens *)
let collect (lex : Lexer.t) : (Token.t list) =
    let rec aux (lex : Lexer.t) : Token.t list =
        begin match (Lexer.get_char lex) with
            | '~' -> let (l2, t) = Lexer.next_token lex in
                [t]
            | _ -> let (l2, t) = Lexer.next_token lex in
                (* Printf.printf "Token : %s\n" (Token.token_to_string t); *)
                if t = Token.EOF then 
                    [t]
                else
                    t :: aux l2
        end
    in
    aux lex


let display_token_list (tl : Token.t list) =
    let tls = List.map Token.token_to_string tl in
    List.iter print_endline tls


let small = "=+(){},;"


let small_expected = [
    Token.ASSIGN;
    Token.PLUS;
    Token.LPAREN;
    Token.RPAREN;
    Token.LBRACE;
    Token.RBRACE;
    Token.COMMA;
    Token.SEMICOLON;
    Token.EOF;
]


let large = "
let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
"

let large_expected = [
    Token.LET;
    Token.IDENT "five";
    Token.ASSIGN;
    Token.INT "5";
    Token.SEMICOLON;
    Token.LET;
    Token.IDENT "ten";
    Token.ASSIGN;
    Token.INT "10";
    Token.SEMICOLON;
    Token.LET;
    Token.IDENT "add";
    Token.ASSIGN;
    Token.FUNCTION;
    Token.LPAREN;
    Token.IDENT "x";
    Token.COMMA;
    Token.IDENT "y";
    Token.RPAREN;
    Token.LBRACE;
    Token.IDENT "x";
    Token.PLUS;
    Token.IDENT "y";
    Token.SEMICOLON;
    Token.RBRACE;
    Token.SEMICOLON;
    Token.LET;
    Token.IDENT "result";
    Token.ASSIGN;
    Token.IDENT "add";
    Token.LPAREN;
    Token.IDENT "five";
    Token.COMMA;
    Token.IDENT "ten";
    Token.RPAREN;
    Token.SEMICOLON;
    Token.BANG;
    Token.MINUS;
    Token.SLASH;
    Token.ASTERISK;
    Token.INT "5";
    Token.SEMICOLON;
    Token.INT "5";
    Token.LT;
    Token.INT "10";
    Token.GT;
    Token.INT "5";
    Token.SEMICOLON;
    Token.IF;
    Token.LPAREN;
    Token.INT "5";
    Token.LT;
    Token.INT "10";
    Token.RPAREN;
    Token.LBRACE;
    Token.RETURN;
    Token.TRUE;
    Token.SEMICOLON;
    Token.RBRACE;
    Token.ELSE;
    Token.LBRACE;
    Token.RETURN;
    Token.FALSE;
    Token.SEMICOLON;
    Token.RBRACE;
    Token.INT "10";
    Token.EQ;
    Token.INT "10";
    Token.SEMICOLON;
    Token.INT "10";
    Token.NOT_EQ;
    Token.INT "9";
    Token.SEMICOLON;
    Token.EOF;
]


let tests = "test suite for lexer" >::: [
	"small" >:: (fun _ -> 
		let l = Lexer.init small in
        let output = collect l in
        (*
        display_token_list small_expected;
        print_endline "\n";
        display_token_list output;
        print_endline "\n";
        *)
		assert_equal output small_expected
    );
	"large" >:: (fun _ -> 
		let l = Lexer.init large in
        let output = collect l in
        (*
        display_token_list large_expected;
        print_endline "\n";
        display_token_list output;
        print_endline "\n";
        *)
		assert_equal output large_expected
    )
]

let _ = run_test_tt_main tests
