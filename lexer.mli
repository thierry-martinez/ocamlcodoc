exception Syntax_error of Lexing.position * string

val main : out_channel -> Lexing.lexbuf -> bool

val doc_comment : Lexing.position -> out_channel -> Lexing.lexbuf -> bool
