open Batteries

let spec = []
let src = Global.empty "src"      (* trace name *)
let prec = Global.empty "prec"
let usage = "Usage: main.native [<options>] <Trace File> \n<options> are: "
let run () =
  let _ = Arg.parse spec
    (fun x -> if Sys.file_exists x then Global.set src x
      else raise (Arg.Bad (x^": No such file"))) usage in
  try
    Error.init ();
    let lexbuf =
      Lexing.from_channel
        (if not (Global.isdef src) then
            stdin
         else open_in (Global.get_exn src)) in
    let out = IO.stdout in
    let (p, fs, pt) = Parser.main Lexer.start lexbuf in
    begin
      Global.set prec p;
      String.print out "Require Import Reals.\nRequire Import Interval_tactic.\n\n";
      Ptree.check pt fs;
      Ptree.print_log stderr
    end
  with v ->
    Error.handle_exn v

let x = Printexc.catch run ()
