{
  type token =
    | INT of int
    | ID of string
    | KEYWORD of string
    | BOOL_OP of string
    | ARITH_OP of string
    | COMP_OP of string
    | STR_OP of string
    | LPAREN
    | RPAREN
    | COMMA
    | WHITESPACE
    | EOF
    | ERROR of string
}

let digit = ['0'-'9']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let prime = '\''
let underscore = '_'
let quotes = '"'
let whitespace = [' ' '\t' '\n']

let number = digit+
let keyword = "if" | "then" | "else" | "true" | "false" | "pair"
let identifier = (lower_letter | underscore) (lower_letter | digit | prime | underscore | upper_letter)*
let bool_op = "&&" | "||" | "not"
let lparen = '('
let rparen = ')'
let comma = ','
let arithmetic_op = "+" | "-" | "*" | "/"
let comparison_op = "=" | "!=" | "<" | ">" | "<=" | ">="
let stringdef = (quotes) (lower_letter | digit | prime | underscore | upper_letter | whitespace)* (quotes)

rule token = parse
  | number            { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | keyword           { KEYWORD(Lexing.lexeme lexbuf) }
  | identifier        { ID(Lexing.lexeme lexbuf) }
  | bool_op           { BOOL_OP(Lexing.lexeme lexbuf) }
  | arithmetic_op     { ARITH_OP(Lexing.lexeme lexbuf) }
  | comparison_op     { COMP_OP(Lexing.lexeme lexbuf) }
  | lparen            { LPAREN }
  | rparen            { RPAREN }
  | comma             { COMMA }
  | whitespace        { WHITESPACE }
  | stringdef         { STR_OP(Lexing.lexeme lexbuf) }
  | eof               { EOF }
  | _                 { ERROR("Invalid token: " ^ Lexing.lexeme lexbuf) } (* Handle ERROR *)

 
{
let test_tokenize input_string =
  let lexbuf = Lexing.from_string input_string in
  let rec tokenize acc =
    let token = token lexbuf in
    match token with
    | EOF -> List.rev acc
    | tok -> tokenize (tok :: acc)
  in
  tokenize []


let () =
  let input =  "A12 + !arr = 2are" in
  let tokens = test_tokenize input in
  List.iter (fun tok -> 
    let token_str = match tok with
      | INT i -> "INT " ^ string_of_int i
      | ID s -> "IDENTIFIER " ^ s
      | KEYWORD s -> "KEYWORD " ^ s
      | BOOL_OP s -> "BOOL_OP " ^ s
      | ARITH_OP s -> "ARITH_OP " ^ s
      | COMP_OP s -> "COMP_OP " ^ s
      | STR_OP s -> "STRING " ^ s 
      | LPAREN -> "LPAREN"
      | RPAREN -> "RPAREN"
      | COMMA -> "COMMA"
      | WHITESPACE -> ""
      | EOF -> ""
      | ERROR e -> "ERROR " ^ e  (* Handle ERROR *)
    in
    if token_str <> "" then
      print_endline token_str) tokens

(* Test Cases
 if e12 >= 42 * (y1 - 3 + 4) != 1 then true else x1;
if (223 - 343) > (421 * 434) then "hello world" else "sorry world";
if (x + y * z) / 2 >= 100 || (a - b) * c < 50 then "valid" else "invalid";
A12 + !arr = 2are;
 *)

}


