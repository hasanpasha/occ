let src = Logs.Src.create "occ.lexer" ~doc:"logs occ's lexer events"

module Log = (val Logs.src_log src : Logs.LOG)
open Token

type t = { input : string; ch : char option; pos : int } [@@deriving show]

let init input =
  {
    input;
    ch = (if String.length input = 0 then None else Some (String.get input 0));
    pos = 0;
  }

module Char = struct
  include Char

  let is_whitespace ch =
    ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r' || ch = '\x0c'

  let is_alpha ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
  let is_digit ch = ch >= '0' && ch <= '9'
end

let rec lex lexer =
  let rec to_seq next state () =
    match next state with
    | state', Some tok -> Seq.Cons (tok, to_seq next state')
    | _, None -> Seq.Nil
  in
  to_seq next_token lexer

and next_token lexer =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (lexer, None)
  | Some ch ->
      let sym1 tok = (advance lexer, tok) in
      let sym2 tok = (advance_by lexer 2, tok) in
      let sym3 tok = (advance_by lexer 3, tok) in
      let peek_is ch = peek_char lexer = Some ch in
      let peek_are ch1 ch2 =
        peek_char lexer = Some ch1 && peek_next_char lexer = Some ch2
      in
      let lexer, token =
        match ch with
        | '(' -> sym1 LParen
        | ')' -> sym1 RParen
        | '{' -> sym1 LCub
        | '}' -> sym1 RCub
        | ';' -> sym1 Semi
        | '~' -> sym1 Tilde
        | '!' when peek_is '=' -> sym2 ExclEqual
        | '!' -> sym1 Excl
        | '+' when peek_is '+' -> sym2 PlusPlus
        | '+' when peek_is '=' -> sym2 PlusEqual
        | '+' -> sym1 Plus
        | '-' when peek_is '-' -> sym2 MinusMinus
        | '-' when peek_is '=' -> sym2 MinusEqual
        | '-' -> sym1 Minus
        | '*' when peek_is '=' -> sym2 AstrskEqual
        | '*' -> sym1 Astrsk
        | '/' when peek_is '=' -> sym2 SlashEqual
        | '/' -> sym1 Slash
        | '%' when peek_is '=' -> sym2 PercntEqual
        | '%' -> sym1 Percnt
        | '&' when peek_is '&' -> sym2 AmpAmp
        | '&' when peek_is '=' -> sym2 AmpEqual
        | '&' -> sym1 Amp
        | '^' when peek_is '=' -> sym2 HatEqual
        | '^' -> sym1 Hat
        | '|' when peek_is '|' -> sym2 VerbarVarbar
        | '|' when peek_is '=' -> sym2 VerbarEqual
        | '|' -> sym1 Verbar
        | '<' when peek_are '<' '=' -> sym3 LtLtEqual
        | '<' when peek_is '<' -> sym2 LtLt
        | '<' when peek_is '=' -> sym2 LtEqual
        | '<' -> sym1 Lt
        | '>' when peek_are '>' '=' -> sym3 GtGtEqual
        | '>' when peek_is '>' -> sym2 GtGt
        | '>' when peek_is '=' -> sym2 GtEqual
        | '>' -> sym1 Gt
        | '=' when peek_is '=' -> sym2 EqualEqual
        | '=' -> sym1 Equal
        | '?' -> sym1 Quest
        | ':' -> sym1 Colon
        | ',' -> sym1 Comma
        | _ when is_identifier_start ch -> lex_identifier lexer
        | _ when Char.is_digit ch -> lex_number lexer
        | _ ->
            Log.err (fun m -> m "unrecognized character: %c" ch);
            failwith (Printf.sprintf "unknown char: %c" ch)
      in
      (lexer, Some token)

and is_identifier_start ch = Char.is_alpha ch || ch = '_'
and is_identifier ch = is_identifier_start ch || Char.is_digit ch

and lex_identifier lexer =
  let lexer, ident = read_while lexer is_identifier in
  ( lexer,
    match ident with
    | "static" -> Static
    | "extern" -> Extern
    | "int" -> Int
    | "void" -> Void
    | "return" -> Return
    | "if" -> If
    | "else" -> Else
    | "goto" -> Goto
    | "while" -> While
    | "do" -> Do
    | "for" -> For
    | "break" -> Break
    | "continue" -> Continue
    | "switch" -> Switch
    | "case" -> Case
    | "default" -> Default
    | _ -> Ident ident )

and lex_number lexer =
  let lexer, lexeme = read_while lexer Char.is_digit in
  let lexer, suffix = read_while lexer is_identifier in
  match suffix with
  | _ when String.length suffix > 0 -> failwith "invalid suffix"
  | _ -> (lexer, IntLit (int_of_string lexeme))

and skip_whitespace lexer =
  let lexer, _ =
    seek lexer (fun ch ->
        match ch with Some ch -> Char.is_whitespace ch | None -> false)
  in
  lexer

and read_while lexer pred =
  let start_pos = lexer.pos in
  let lexer, end_pos =
    seek lexer (fun ch ->
        match ch with Some char -> pred char | None -> false)
  in
  (lexer, String.sub lexer.input start_pos (end_pos - start_pos))

and seek lexer pred =
  let rec seek' lexer =
    match pred lexer.ch with
    | true -> seek' (advance lexer)
    | false -> (lexer, lexer.pos)
  in
  seek' lexer

and advance_by lexer offset =
  let { input; ch; pos } = lexer in
  match ch with
  | None -> lexer
  | Some _ ->
      let ch = peek_char_by lexer offset in
      let pos = pos + offset in
      { input; ch; pos }

and advance lexer = advance_by lexer 1

and peek_char_by lexer offset =
  let next_pos = lexer.pos + offset in
  match next_pos >= String.length lexer.input with
  | true -> None
  | false -> Some (String.get lexer.input next_pos)

and peek_char lexer = peek_char_by lexer 1
and peek_next_char lexer = peek_char_by lexer 2
