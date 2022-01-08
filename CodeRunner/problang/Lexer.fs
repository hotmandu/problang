module Problang.Lexer

type Token =
    | Null
    | Bad
    | EOF
    | Comment

    | String

let rec public Lex (content: string): Token seq = 
    if content = "" then
        Seq.empty<Token>
    else
        seq {
            yield Null
            yield! Lex content
        }