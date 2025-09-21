open Occ
open Cmdliner

type options = {
  verbose : bool;
  lex : bool;
  parse : bool;
  validate : bool;
  tacky : bool;
  codegen : bool;
  compile : bool;
  arch : Air_lower.arch;
  input : string;
}

let replace_extension filename new_ext =
  try
    let base = Filename.chop_extension filename in
    base ^ "." ^ new_ext
  with Invalid_argument _ -> filename ^ "." ^ new_ext

let occ (options : options) =
  Logs.set_reporter (Logs_fmt.reporter ());

  if options.verbose then Logs.set_level (Some Logs.Debug)
  else Logs.set_level (Some Logs.Info);

  let cc = replace_extension options.input "cc" in

  let expand_cmd_code =
    Sys.command (Printf.sprintf "gcc -P -E %s -o %s" options.input cc)
  in
  if expand_cmd_code != 0 then exit expand_cmd_code
  else
    let input = In_channel.with_open_text cc In_channel.input_all in

    let lexer = Lexer.init input in

    if options.lex then (
      let seq = Lexer.lex lexer in
      Seq.iter (fun tok -> Logs.info (fun m -> m "%s" (Token.show tok))) seq;
      exit 0);

    let program = Parser.parse_from_lexer lexer in

    if options.parse then (
      Logs.info (fun m -> m "%s" (Ast.show program));
      exit 0);

    let vir = Validate.validate program in

    if options.validate then (
      Logs.info (fun m -> m "%s" (Ast.show vir));
      exit 0);

    let ir = Tacky_ir_lower.lower vir in

    if options.tacky then (
      Logs.info (fun m -> m "%s" (Tacky_ir.show ir));
      exit 0);

    let air = Air_lower.lower ir options.arch in

    if options.codegen then (
      Logs.info (fun m -> m "%s" (Air.show air));
      exit 0);

    let s = replace_extension options.input "s" in
    Out_channel.with_open_text s (fun oc -> output_string oc (Air.emit air));

    let obj = replace_extension options.input "o" in
    if Sys.command (Printf.sprintf "gcc -c %s -o %s" s obj) != 0 then exit 1;

    if options.compile then exit 0;

    let exe = Filename.chop_extension options.input in
    if Sys.command (Printf.sprintf "gcc %s -o %s" obj exe) != 0 then exit 1

let arch_conv : Air_lower.arch Arg.conv =
  let parse = function
    | "x86_64" -> `Ok Air_lower.X86_64
    | s -> `Error ("Unknown architecture: " ^ s)
  in
  let print fmt a =
    let s = match a with Air_lower.X86_64 -> "x86_64" in
    Format.fprintf fmt "%s" s
  in
  (parse, print)

let occ_cmd =
  let input =
    let doc = "Input file" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"INPUT" ~doc)
  in
  let verbose =
    Arg.(
      value & flag & info [ "verbose" ] ~docv:"VEROSE" ~doc:"Print all messages")
  in
  let lex =
    Arg.(value & flag & info [ "lex" ] ~docv:"LEX" ~doc:"lex and exit")
  in
  let parse =
    Arg.(value & flag & info [ "parse" ] ~docv:"PARSE" ~doc:"parse and exit")
  in
  let validate =
    Arg.(
      value & flag
      & info [ "validate" ] ~docv:"VALIDATE" ~doc:"validate and exit")
  in
  let tacky =
    Arg.(
      value & flag
      & info [ "tacky" ] ~docv:"TACKY" ~doc:"generate tacky ir and exit")
  in
  let codegen =
    Arg.(
      value & flag
      & info [ "codegen" ] ~docv:"CODEGEN"
          ~doc:"generate asm and exit without emit")
  in
  let compile =
    Arg.(
      value & flag
      & info [ "compile" ] ~docv:"COMPILE"
          ~doc:"compile asm to object file and exit without linking")
  in
  let arch =
    let doc = "target architecture (x86_64)" in
    Arg.(value & opt arch_conv Air_lower.X86_64 & info [ "t"; "target" ] ~doc)
  in
  let opts_term =
    Term.(
      const (fun verbose lex parse validate tacky codegen compile arch input ->
          {
            verbose;
            lex;
            parse;
            validate;
            tacky;
            codegen;
            compile;
            arch;
            input;
          })
      $ verbose $ lex $ parse $ validate $ tacky $ codegen $ compile $ arch
      $ input)
  in
  let doc = "Compile a subset of c language program using occ compiler" in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <bashahsn22@gmail.com>." ]
  in
  let term = Term.(const occ $ opts_term) in
  Cmd.v (Cmd.info "occ" ~version:"0.1.0" ~doc ~man) term

let main () = Cmd.eval occ_cmd
let () = if !Sys.interactive then () else exit (main ())
