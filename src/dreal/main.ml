open Batteries

let _ = Arith_status.set_error_when_null_denominator false
let spec = []
let src = Global.empty "src"      (* trace name *)
let prec = Global.empty "prec"
let coqlib = ref true
let usage = "Usage: main.native [<options>] <Trace File> \n<options> are:"
let args = Arg.align [
	       ("-i", Arg.Set coqlib, "    Using Coq.Interval (default)");
	       ("-g", Arg.Clear coqlib, "    Using Gappa (default)")]
let run () =
  let _ = Arg.parse args
    (fun x -> if Sys.file_exists x then Global.set src x
      else raise (Arg.Bad (x^": No such file"))) usage in
  try
    Error.init ();
    let lexbuf =
      Lexing.from_channel
        (if not (Global.isdef src) then
            stdin
         else open_in (Global.get_exn src)) in
    let dest_file = 
      let src_file = Global.get_exn src in
      if Filename.check_suffix src_file ".smt2.proof" then
	Filename.chop_suffix src_file ".smt2.proof"
      else
	try
	  Filename.chop_extension src_file
	with Invalid_argument _ -> src_file in
    let list_proofs = Parser.main Lexer.start lexbuf in
    let total = List.length list_proofs in
    let ck_proof p fs pt num =
      let out = 
	if total > 1 then
	  open_out (dest_file^"_"^(string_of_int (num+1))^".v")
	else
	  open_out (dest_file^".v") in
      begin
	try
	  Global.set prec p;
	  if !coqlib then
	    String.print out "Require Import Reals.\nRequire Import Interval_tactic.\nOpen Scope R_scope.\n"
	  else
	    String.print out "Require Import Reals.\nRequire Import Gappa_tactic.\nOpen Scope R_scope.\n";	
	  Ptree.check out pt fs;
	  Ptree.print_log stderr;
	  Ptree.reset_log();
	  close_out(out);
	  num + 1
	with v ->
	  (match v with
	   | Ptree.Error s -> print_endline s;
	   | Basic.NOTINCOQ s -> print_endline (s^" is not yet supported!");
	   | _ -> print_endline ("error "^(Printexc.to_string v)^" in proof no. "^(string_of_int (num+1))));
	  Ptree.print_log stderr;
	  Ptree.reset_log();
	  close_out(out);
	  num + 1
      end in
    List.fold_left (fun n (p, fs, pt) -> ck_proof p fs pt n) 0 list_proofs
  with v ->
    Error.handle_exn v

let x =
  Printexc.catch run ()
