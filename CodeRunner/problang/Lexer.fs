module Problang.Lexer
open System.Text.RegularExpressions

type Token =
    | Nothing
    | Bad

    | EOF
    | Newline
    | Whitespace of int
    | Comment

    | Record of string
    | String of string

let rec private advanceIndex (text: string) pred idx =
    if text.Length <= idx || not (pred text idx) then
        idx
    else
        advanceIndex text pred (idx + 1)

let rgx = Regex "qfqf"

let rec public Lex (content: string) : Token seq =
    seq {
        if content = "" then
            yield EOF
            yield! Lex content
        elif content.[0] = '\n' then
            yield Newline
            yield! Lex(content.Substring 1)
        elif content.[0] = '\r' then
            if content.Length > 2 && content.[1] = '\n' then
                yield Newline
                yield! Lex(content.Substring 2)
            else
                yield Newline
                yield! Lex(content.Substring 1)
        elif content.[0] = ' ' || content.[0] = '\t' then
            let wc =
                advanceIndex content (fun x i -> x.[i] = ' ' || x.[i] = '\t') 0

            yield Whitespace wc
            yield! Lex(content.Substring wc)
        elif content.Length > 2
             && content.[0] = '/'
             && content.[1] = '/' then
            let nlc =
                advanceIndex content (fun x i -> x.[i] <> '\r' && x.[i] <> '\n') 0

            yield Comment
            yield! Lex(content.Substring nlc)
        elif content.[0] = '"' then
            //TODO: support escaping \"
            let qc =
                (advanceIndex content (fun x i -> x.[i] <> '"') 1)
                + 1

            if qc > content.Length
               || (qc = content.Length && content.[qc - 1] <> '"') then
                yield Bad
                yield! Lex ""
            else
                yield String(content.Substring(0, qc))
                yield! Lex(content.Substring qc)
        else
            let rc =
                advanceIndex content (fun x i -> x.[i] <> ' ' && x.[i] <> '\t' && x.[i] <> '\r' && x.[i] <> '\n') 1

            yield Record(content.Substring(0, rc))
            yield! Lex(content.Substring rc)
    }
