open Batteries

exception CException of string

type key = string
let keys = Map.keys

type nintv = Intv.nt
type nt = (key, nintv) Map.t

let from_nlist (l : (key * nintv) list) : nt =
  List.fold_left
    (fun e (k, i) -> Map.add k i e)
    Map.empty
    l

let to_nlist (e : nt) : (key * nintv) list
    = List.of_enum (Map.backwards e)

let nmake = from_nlist

let coq_nprint_def out (e : nt) i =
  let nenv_list = to_nlist e in
  let print_nenv out (x, {Intv.nlow=l; Intv.nhigh=h}) =
(*    Printf.fprintf out "(%s <= %s <= %s)%%R" (Num.to_string l) x (Num.to_string h) in *)
    Printf.fprintf out "%s <= %s <= %s" (Num.to_string l) x (Num.to_string h) in
  let _ =
    List.print ~first:("\nDefinition bounded_"^(string_of_int i)^" ") ~last:" :=\n" ~sep:" "
	       String.print
	       out
	       (List.of_enum (keys e)) in
  List.print ~first:"" ~last:". \n" ~sep:" /\\\n"
	     print_nenv
	     out
	     nenv_list

let coq_nprint out (e : nt) i  =
  let _ = 
    List.print ~first:"forall " ~last:",\n" ~sep:" "
	       String.print
	       out
	       (List.of_enum (keys e)) in
  List.print ~first:("bounded_"^(string_of_int i)^" ") ~last:" ->\n" ~sep:" "
	     String.print
	     out
	     (List.of_enum (keys e))

let nfind (x : key) (e : nt) : nintv
    = Map.find x e

let norder (e1 : nt) (e2 : nt) : bool =
  Map.for_all
    (fun x i1 ->
      let i2 = nfind x e2 in
      Intv.norder i1 i2
    )
    e1

let njoin (e1 : nt) (e2 : nt) : nt =
  Map.merge
    (fun x i1_op i2_op ->
      match (i1_op, i2_op) with
          (Some i1, Some i2) -> Some (Intv.njoin i1 i2)
        | _ -> raise (CException "Merge fail"))
    e1
    e2

let nequals (e1 : nt) (e2 : nt) : bool =
  not (List.mem
       false
       (List.map
          (fun ((_, i1), (_, i2)) ->
            Intv.nequals i1 i2)
          (List.combine (to_nlist e1) (to_nlist e2))))

let nprint out =
  Map.print ~first:"" ~last:"\n" ~sep:", \n"
    String.print
    Intv.nprint
    out

let is_nempty (e : nt) : bool =
  List.mem true
    (List.map
       (fun (_, {Intv.nlow = l; Intv.nhigh = h})
       -> (Num.eq_num l h))
       (to_nlist e))

(* minus e1 e2 == (e1 - e2) *)
let nminus (e1 : nt) (e2 : nt) : (nt list) =
  let extract_diff_keys l1 l2 =
    let diff_list =
      List.filter
	(fun ((_, i1), (_, i2))
	 -> not (Intv.nequals i1 i2))
	(List.combine l1 l2) in
    List.map (fun ((key, _), (_, _)) -> key) diff_list
  in
  let l1 = to_nlist e1 in
  let l2 = to_nlist e2 in
  let l12 = List.combine l1 l2 in
  let diff_keys = extract_diff_keys l1 l2 in
  let subt diff_key =
    let (l1', l2') =
      List.split
	(List.map
           (fun (((key1, {Intv.nlow = l1; Intv.nhigh = h1}) as elem1),
		 ((key2, {Intv.nlow = l2; Intv.nhigh = h2}) as elem2))
            ->
            if key1 != diff_key then
              (elem1, elem1)
            else
              ((key1, {Intv.nlow = l1; Intv.nhigh = l2}),
               (key2, {Intv.nlow = h2; Intv.nhigh = h1}))
           )
           l12) in
    [l1'; l2'] in
  let subt_nenvs = [] in
  List.filter (fun e -> not (is_nempty e))
  	      (List.map (fun l -> nmake l) 
			(List.fold_left
			   (fun l k -> (List.append l (subt k)))
			   subt_nenvs
			   diff_keys))

