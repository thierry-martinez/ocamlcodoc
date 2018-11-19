{
  exception Syntax_error of Lexing.position * string
}

let ident = ['a' - 'z' '_'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_']*

rule main out_channel = parse
| "(**" {
  doc_comment lexbuf.lex_curr_p out_channel lexbuf
}
| "(*" {
  code_comment lexbuf.lex_curr_p None lexbuf;
  main out_channel lexbuf
}
| "\"" {
  string lexbuf.lex_curr_p None lexbuf;
  main out_channel lexbuf;
}
| "{" (ident as delim) "|" {
  ident_string lexbuf.lex_curr_p None delim lexbuf;
  main out_channel lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  main out_channel lexbuf
}
| eof {
  false
}
| _ {
  main out_channel lexbuf
}
and doc_comment start_pos out_channel = parse
| "*)" {
  main out_channel lexbuf
}
| "{[" {
  let start_pos = lexbuf.lex_curr_p in
  Utils.output_position out_channel start_pos;
  codoc start_pos out_channel lexbuf
}
| "(*" {
  code_comment lexbuf.lex_curr_p None lexbuf;
  doc_comment start_pos out_channel lexbuf
}
| "\"" {
  string lexbuf.lex_curr_p None lexbuf;
  doc_comment start_pos out_channel lexbuf
}
| "{" (ident as delim) "|" {
  ident_string lexbuf.lex_curr_p None delim lexbuf;
  main out_channel lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  doc_comment start_pos out_channel lexbuf
}
| eof {
  raise (Syntax_error (start_pos, "Unterminated doc-comment"))
}
| _ {
  doc_comment start_pos out_channel lexbuf
}
and codoc start_pos out_channel = parse
| "]}" {
  true
}
| "\n" {
  Lexing.new_line lexbuf;
  output_char out_channel '\n';
  codoc start_pos out_channel lexbuf
}
| "(*" {
  output_string out_channel "(*";
  code_comment lexbuf.lex_curr_p (Some out_channel) lexbuf;
  codoc start_pos out_channel lexbuf
}
| "\"" {
  output_string out_channel "\"";
  string lexbuf.lex_curr_p (Some out_channel) lexbuf;
  codoc start_pos out_channel lexbuf
}
| ("{" (ident as delim) "|") as start_string {
  output_string out_channel start_string;
  ident_string lexbuf.lex_curr_p (Some out_channel) delim lexbuf;
  codoc start_pos out_channel lexbuf
}
| "*)" {
  raise (Syntax_error (lexbuf.lex_curr_p, "Unterminated code in doc-comment"))
}
| eof {
  raise (Syntax_error (start_pos, "Unterminated code in doc-comment"))
}
| _ as char {
  output_char out_channel char;
  codoc start_pos out_channel lexbuf
}
and code_comment start_pos out_channel = parse
| "(*" {
  Utils.option_iter (fun out_channel -> output_string out_channel "(*")
      out_channel;
  code_comment start_pos out_channel lexbuf;
  code_comment start_pos out_channel lexbuf
}
| "*)" {
  Utils.option_iter (fun out_channel -> output_string out_channel "*)")
      out_channel;
  ()
}
| "\"" {
  Utils.option_iter (fun out_channel -> output_string out_channel "\"")
      out_channel;
  ignore (string lexbuf.lex_curr_p out_channel lexbuf);
  code_comment start_pos out_channel lexbuf
}
| "\n" {
  Utils.option_iter (fun out_channel -> output_string out_channel "\n")
      out_channel;
  Lexing.new_line lexbuf;
  code_comment start_pos out_channel lexbuf
}
| eof {
  raise (Syntax_error (start_pos, "Unterminated code comment"))
}
| _ {
  code_comment start_pos out_channel lexbuf
}
and string start_pos out_channel = parse
| "\"" {
  Utils.option_iter (fun out_channel -> output_string out_channel "\"")
      out_channel;
}
| ("\\" _) as s {
  Utils.option_iter (fun out_channel -> output_string out_channel s)
      out_channel;
  string start_pos out_channel lexbuf
}
| "\n" {
  Utils.option_iter (fun out_channel -> output_string out_channel "\n")
      out_channel;
  Lexing.new_line lexbuf;
  string start_pos out_channel lexbuf
}
| eof {
  raise (Syntax_error (start_pos, "Unterminated string"))
}
| _ as char {
  Utils.option_iter (fun out_channel -> output_char out_channel char)
      out_channel;
  string start_pos out_channel lexbuf
}
and ident_string start_pos out_channel delim = parse
| ("|" (ident as delim') "}") as end_string {
  Utils.option_iter (fun out_channel -> output_string out_channel end_string)
      out_channel;
  if delim = delim' then
    ()
  else
    ident_string start_pos out_channel delim lexbuf
}
| ("\\" _) as s {
  Utils.option_iter (fun out_channel -> output_string out_channel s)
      out_channel;
  ident_string start_pos out_channel delim lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  Utils.option_iter (fun out_channel -> output_string out_channel "\n")
      out_channel;
  ident_string start_pos out_channel delim lexbuf
}
| eof {
  raise (Syntax_error (start_pos, "Unterminated string"))
}
| _ as char {
  Utils.option_iter (fun out_channel -> output_char out_channel char)
      out_channel;
  ident_string start_pos out_channel delim lexbuf
}
