open Occ
open Cmdliner

type options =
{
  verbose: bool;
  lex: bool;
  parse: bool;
  validate: bool;
  tacky: bool;
  codegen: bool;
  input: string
}

let replace_extension filename new_ext =
  try
    let base = Filename.chop_extension filename in
    base ^ "." ^ new_ext
  with Invalid_argument _ ->
    filename ^ "." ^ new_ext

let occ (options: options) =
  Logs.set_reporter (Logs_fmt.reporter ());

  if options.verbose then
    Logs.set_level (Some Logs.Debug)
  else 
    Logs.set_level (Some Logs.Info);

  let cc = replace_extension options.input "cc" in
  
  let expand_cmd_code = Sys.command (Printf.sprintf "gcc -P -E %s -o %s" options.input cc) in
  if expand_cmd_code != 0 then
    exit(expand_cmd_code)
  else
    
  let input = In_channel.with_open_text cc In_channel.input_all in
  
  let lexer = Lexer.init input in

  if options.lex then begin
    let seq = Lexer.lex lexer in
    Seq.iter  (fun tok -> Logs.info (fun m -> m "%s" (Token.show tok))) seq;
    exit(0)
  end;
    
  let program = Parser.parse_from_lexer lexer in

  if options.parse then begin
    Logs.info (fun m -> m "%s" (Ast.show program));
    exit(0)
  end;

  let vir = Validate.validate program in

  if options.validate then begin 
    Logs.info (fun m -> m "%s" (Vir.show vir));
    exit(0)
  end;

  let ir = Tacky_ir_lower.lower vir in

  if options.tacky then begin
    Logs.info (fun m -> m "%s" (Tacky_ir.show ir));
    exit(0)
  end;

  if options.codegen then begin
    ()
  end;

  ()

let occ_cmd =
  let input =
    let doc = "Input file" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"INPUT" ~doc)
  in
  let verbose =
    Arg.(value & flag & info ["verbose"] ~docv:"VEROSE" ~doc:"Print all messages")
  in
  let lex =
    Arg.(value & flag & info ["lex"] ~docv:"LEX" ~doc:"lex and exit")
  in
  let parse =
    Arg.(value & flag & info ["parse"] ~docv:"PARSE" ~doc:"parse and exit")
  in
  let validate =
    Arg.(value & flag & info ["validate"] ~docv:"VALIDATE" ~doc:"validate and exit")
  in
  let tacky =
    Arg.(value & flag & info ["tacky"] ~docv:"TACKY" ~doc:"generate tacky ir and exit")
  in
  let codegen =
    Arg.(value & flag & info ["codegen"] ~docv:"CODEGEN" ~doc:"generate asm and exit without emit")
  in
  let opts_term =
  Term.(const (fun verbose lex parse validate tacky codegen input ->
      { verbose; lex; parse; validate; tacky; codegen; input })
    $ verbose $ lex $ parse $ validate $ tacky $ codegen $ input)
  in
  let doc = "Compile a subset of c language program using occ compiler" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <bashahsn22@gmail.com>." ]
  in
  let term = Term.(const occ $ opts_term) in
  Cmd.v (Cmd.info "occ" ~version:"0.1.0" ~doc ~man) term

let main () = Cmd.eval occ_cmd
let () = if !Sys.interactive then () else exit (main ())