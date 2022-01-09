module Problang.Lexer

open System.Text.RegularExpressions

type Token =
    | Nothing
    | Bad

    | EOF
    | Newline
    | Whitespace of int
    | Comment

    | String of string
    | Record of string
    
let rec private advanceIndex (text: string) pred idx =
    if text.Length <= idx || not (pred text idx) then
        idx
    else
        advanceIndex text pred (idx + 1)

let rec private advanceByRegex (text: string) regex =
    let m = Regex("^" + regex).Match(text)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

// Comsume builder
type ConsumeBuilder() =
    member this.Zero() = None
    member this.Return(x) = Some x

let private consume = new ConsumeBuilder()

let private consumeEOF (text: string) =
    consume { if text = "" then return EOF, text }

let private consumeNewline (text: string) =
    consume {
        if text.[0] = '\n' then
            return Newline, (text.Substring 1)
        elif text.[0] = '\r' then
            if text.Length > 2 && text.[1] = '\n' then
                return Newline, (text.Substring 2)
            else
                return Newline, (text.Substring 1)
    }

let private consumeWhitespace (text: string) =
    consume {
        if text.[0] = ' ' || text.[0] = '\t' then
            let wc =
                advanceIndex text (fun x i -> x.[i] = ' ' || x.[i] = '\t') 0

            return Whitespace wc, (text.Substring wc)
    }

let private consumeComment (text: string) =
    consume {
        if text.Length > 2 && text.StartsWith("//") then
            let nlc =
                advanceIndex text (fun x i -> x.[i] <> '\r' && x.[i] <> '\n') 0

            return Comment, (text.Substring nlc)
    }

let private consumeString (text: string) =
    consume {
        if text.[0] = '"' then
            match advanceByRegex text "\"((?:[^\"\\\\]|\\\\[\\s\\S])*)\"" with
            | Some(v) -> return String(v.[0]), (text.Substring (2 + v.[0].Length))
            | None -> return Bad, ""
    }

let private consumeRecord (text: string) =
    consume {
        let rc =
            advanceIndex
                text
                (fun x i ->
                    x.[i] <> ' '
                    && x.[i] <> '\t'
                    && x.[i] <> '\r'
                    && x.[i] <> '\n')
                1

        return Record(text.Substring(0, rc)), (text.Substring rc)
    }

let private consumerTable: (string -> (Token * string) option) list =
    [ consumeEOF
      consumeNewline
      consumeWhitespace
      consumeComment

      consumeString
      consumeRecord ]

let rec private tryUntilSome (funclist: ('a -> 'b option) list) (arg: 'a) =
    match funclist with
    | hd :: tl ->
        match (hd arg) with
        | None -> tryUntilSome tl arg
        | v -> v
    | [] -> None


/// <summary>
/// Lexical Analyser, converts input string into tokens.
/// </summary>
/// <param name="content">An input string to tokenize</param>
let rec public Lex (content: string) : Token seq =
    match (tryUntilSome consumerTable content) with
    | Some (token, rest) ->
        seq {
            yield token
            yield! Lex rest
        }
    | None -> failwith "Lex Consuming Failure"
