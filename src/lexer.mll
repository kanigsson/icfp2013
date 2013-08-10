{
  open Lexing
  open Parser
  open Programs

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ "not", OP1 Not;
        "shl1", OP1 Shl1;
        "shr1", OP1 Shr1;
        "shr4", OP1 Shr4;
        "shr16", OP1 Shr16;
        "and", OP2 And;
        "or", OP2 Or;
        "xor", OP2 Xor;
        "plus", OP2 Plus;
        "lambda", LAMBDA;
        "if0", IF0;
        "fold", FOLD;
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}


let alpha = ['a'-'z']
let digit = ['0'-'9']
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | "(*"
      { comment lexbuf; token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | '0'
      { INT 0 }
  | '1'
      { INT 1 }
  | _
      { raise (Lexical_error (lexeme lexbuf)) }

and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }

