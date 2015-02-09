open Batteries

type nt = {nlow : Num.num; nhigh : Num.num}

let nmake l h = {nlow=l; nhigh=h}

let ninfinity () = 
  let _ = Arith_status.set_error_when_null_denominator false in
  Num.of_float(infinity)
								   
let nneg_infinity () = 
  let _ = Arith_status.set_error_when_null_denominator false in
  Num.of_float(neg_infinity)

let norder {nlow=l1; nhigh=h1} {nlow=l2; nhigh=h2} =
  (Num.le_num l2 l1) && (Num.le_num h1 h2)

let njoin {nlow=l1; nhigh=h1} {nlow=l2; nhigh=h2} = 
  nmake (Num.min_num l1 l2) (Num.max_num h1 h2)

let nequals {nlow=l1; nhigh=h1} {nlow=l2; nhigh=h2} : bool
    = (Num.eq_num l1 l2) && (Num.eq_num h1 h2)

let nprint out {nlow=l; nhigh=h} =
  Printf.fprintf out "[%s, %s]" (Num.to_string l) (Num.to_string h)
