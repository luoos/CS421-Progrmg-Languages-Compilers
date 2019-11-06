{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)
let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let alpha = ['a' - 'z' 'A' - 'Z']
let alphanum = alpha | numeric
let hexadec = ['a' - 'f'] | numeric

let whitespace = [' ' '\t' '\n']

let iden_c = alphanum | "_" | "'"

rule token = parse
  | [' ' '\t'] { token lexbuf }  (* skip over whitespace *)
  | ['\n']     { token lexbuf }  (* skip over whitespace *)
  | eof        { EOF          }

  | "~"     { NEG }
  | "-"     { MINUS  }
  | "*"     { TIMES  }
  | "/"     { DIV  }
  | "+."    { DPLUS  }
  | "-."    { DMINUS  }
  | "*."    { DTIMES  }
  | "/."    { DDIV  }
  | "^"     { CARAT  }
  | "<"  { LT  }
  | "<=" { LEQ  }
  | ">=" { GEQ  }
  | "="     { EQUALS  }
  | "<>"    { NEQ }
  | "|"     { PIPE  }
  | "->" { ARROW  }
  | "::"    { DCOLON  }
  | ";"     { SEMI }
  | ";;"    { DSEMI }
  | "@"     { AT  }
  | "[]"    { NIL }
  | "let"   { LET  }
  | "and"   { AND}
  | "end"   { END}
  | "in"    { IN  }
  | "if"    { IF  }
  | "then"  { THEN  }
  | "else"  { ELSE  }
  | "mod"   { MOD  }
  | "fun"   { FUN  }
  | "try"   { TRY }
  | "with"  { WITH }
  | "not"   { NOT  }
  | "&&"    { LOGICALAND}
  | "||"    { LOGICALOR}
  | "["     { LBRAC  }
  | "()"    { UNIT }
  | "("     { LPAREN  }
  | ")"     { RPAREN  }
  | ","     { COMMA  }
  | "_"     { UNDERSCORE }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "()"    { UNIT }

  | numeric+ as s         { INT (int_of_string s) }
  | ("0x"(hexadec)+) as s { INT (int_of_string s) }
  | ((numeric+)'.'(numeric*)('e'(numeric)+)?) as s { FLOAT (float_of_string s) }
(* your rules go here *)
  | "+"     { PLUS }
  | ">"     { GT }
  | "]"     { RBRAC }
  | "rec"   { REC }
  | "fun"   { FUN }
  | "raise" { RAISE }
 
  | ("0b" [ '0' '1']+) as s { INT (int_of_string s) }
  | ("0x" [  '0'-'9' 'a'-'f']+) as s { INT (int_of_string s) }
  | (lowercase(iden_c*)) as s { IDENT s }

  | "\"" { STRING (handle_string lexbuf) }

and handle_string = parse
  | "\""                        { "" }
  | "\\"                        { raise (Failure "") }
  | [ '!' ' ' '#'-'~' ] as s    { (String.make 1 s) ^ (handle_string lexbuf) }
  | "\\\""                      { "\"" ^ (handle_string lexbuf) }
  | "\\n"                       { "\n" ^ (handle_string lexbuf) }
  | "\\t"                       { "\t" ^ (handle_string lexbuf) }
  | "\\" (((numeric) (numeric) (numeric)) as value ) { (String.make 1 (char_of_int(int_of_string value))) ^ (handle_string lexbuf) }
  | "\\ "                       { " " ^ (handle_string lexbuf) }
  | "\\\n" [' ' '\t']*          { handle_string lexbuf }
  

{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () = 
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }
