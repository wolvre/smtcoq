open Batteries

let _ = Arith_status.set_error_when_null_denominator false
let src = Global.empty "src"      (* trace name *)
let prec = Global.empty "prec"

let checker fdproof =
  try
    let _ = 
      if Sys.file_exists fdproof then 
	Global.set src fdproof 
      else 
	invalid_arg fdproof in
    let chan = open_in (Global.get_exn src) in
    try
      let _ = Error.init() in
      let lexbuf = Lexing.from_channel chan in
      let list_proofs = Dparser.main Dlexer.start lexbuf in
      let ck_proof p fs pt num =
	begin
	  try
	    Global.set prec p;
	    let _ = Ptree.to_smtcoq pt fs in
	    Ptree.print_log stderr;
	    Ptree.reset_log();
	    num + 1
	  with v ->
	    (match v with
	     | Ptree.Error s -> print_endline s;
	     | Basic.NOTINCOQ s -> print_endline (s^" is not yet supported!");
	     | _ -> print_endline ("error "^(Printexc.to_string v)^" in proof no. "^(string_of_int (num+1))));
	    Ptree.print_log stderr;
	    Ptree.reset_log();
	    num + 1
	end in
      let total = List.fold_left (fun n (p, fs, pt) -> ck_proof p fs pt n) 0 list_proofs in
      print_endline ((string_of_int total)^" out of "^(string_of_int (List.length list_proofs))^" proof(s) checked.")
    with v ->      
      close_in chan;
      Error.handle_exn(v)
  with
    Invalid_argument s->
    print_endline ("Error: cannot find "^s);
