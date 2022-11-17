open OUnit2
open Lexer
open Token

let small = "=+(){},:"
let small_expected = [
	EQ;
	PLUS;
	LPAREN;
	RPAREN;
	LBRACE;
	RBRACE;
	COMMA;
	SEMICOLON;
]

let tests = "test suite for lexer" >::: [
	"small" >:: (fun _ -> 
		let l = init small in
		let rec collect lex acc = 
			match lex.ch with
			| '\x00' -> acc
			| _ -> 
				let (l_next, t) = next_token lex in
				collect l_next (t :: acc)
		in 
		assert_equal (collect (init small) []) small_expected)
]

let _ = run_test_tt_main tests tests
