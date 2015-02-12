(**************************************************************************)
(*                                                                        *)
(*     SMTCoq                                                             *)
(*     Copyright (C) 2011 - 2015                                          *)
(*                                                                        *)
(*     Michaël Armand                                                     *)
(*     Benjamin Grégoire                                                  *)
(*     Chantal Keller                                                     *)
(*                                                                        *)
(*     Inria - École Polytechnique - MSR-Inria Joint Lab                  *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

Require Import Bool.
Require Import List.
Require Import Int63.
Require Import PArray.
Require Import RingMicromega.
Require Import ZMicromega.
Require Import Tauto.
Require Import Psatz.
(* Add LoadPath ".." as SMTCoq. *)

Require Import Misc State.
Require Import SMT_terms.
Require Import SMTCoq.euf.Euf.

Local Open Scope array_scope.
Local Open Scope int63_scope.
Local Open Scope R_scope.

Module Interval
  
  Notation interval := R * R

End Interval

Module ICP_Checker
  
  Inductive step := 
    | ICP0 (intv: list interval)							(* an infeasible intv *)
    | ICP1 (intv: list interval) (intv': list interval) 		      		(* intv' \subseteq intv *) 
    | ICP2 (intv: list interval) (intv1: list interval) (intv2: list interval) 	(* intv \subseteq intv1 \cup intv2 *).

  Definition check_icp0 intv := sorry
  
  Definition check_icp1 intv intv' := sorry

  Definition check_icp2 intv intv1 intv2 := sorry
    
  Definition step_checker t_form (st:step) :=	
    match st with
    | ICP0 intv => check_icp0 t_form intv
    | ICP1 intv intv' => check_icp1 t_form intv intv'
    | ICP2 intv intv1 intv2 => check_icp2 t_form intv intv1 intv2

End ICP_Checker.
