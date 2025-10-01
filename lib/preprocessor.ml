type t = { lexer : Lexer.t; macros : (string, Token.t) Hashtbl.t }

let init lexer = { lexer; macros = Hashtbl.create 32 }

let rec process preprocessor =
  let rec to_seq next state () =
    match next state with
    | state', Some tok -> Seq.Cons (tok, to_seq next state')
    | _, None -> Seq.Nil
  in
  to_seq next_token preprocessor

and next_token preprocessor =
  let open Token in
  let preprocessor, token = lex_next preprocessor in

  let preprocessor, token =
    match token with
    | Some Hash ->
        let preprocessor = process_macro preprocessor in
        next_token preprocessor
    | Some (Ident name) -> (
        match Hashtbl.find_opt preprocessor.macros name with
        | Some value -> (preprocessor, Some value)
        | None -> (preprocessor, token))
    | _ -> (preprocessor, token)
  in

  (preprocessor, token)

and lex_next { lexer; macros } =
  let lexer, token = Lexer.next_token lexer in
  ({ lexer; macros }, token)

and expect_ident preprocessor =
  let preprocessor, next = lex_next preprocessor in
  match next with
  | Some (Ident name) -> (preprocessor, name)
  | _ -> failwith "expected ident token"

and process_macro preprocessor =
  let preprocessor, macro_kind = expect_ident preprocessor in

  match macro_kind with
  | "define" ->
      let preprocessor, name = expect_ident preprocessor in
      let preprocessor, value = next_token preprocessor in

      let value =
        match value with
        | Some value -> value
        | None -> failwith "unexpected end of token stream"
      in

      print_endline [%string "new macro `%{name}`"];
      Hashtbl.replace preprocessor.macros name value;

      preprocessor
  | _ -> failwith [%string "macro kind is unknown yet for me: '%{macro_kind}'"]
