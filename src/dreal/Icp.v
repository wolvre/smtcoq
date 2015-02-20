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

Require Import Reals.
Require Import QArith.
Require Import Bool.
Require Import List.
Require Import Int63.
Require Import PArray.
Require Import RingMicromega.
Require Import ZMicromega.
Require Import RMicromega.
Require Import Tauto.
Require Import Psatz.
(* Add LoadPath ".." as SMTCoq. *)
Add LoadPath "dreal" as SMTCoq.icp.

Require Import Misc State.
Require Import SMT_terms.
Require Import SMTCoq.euf.Euf.

Local Open Scope array_scope.
Local Open Scope int63_scope.
Local Open Scope Q_scope.

Module Icp_Checker.
  
  Inductive interval : Set :=
    | Inan : interval
    | Ilbnd (l : Q) : interval
    | Iubnd (u : Q) : interval
    | Ibnd (l u : Q) : interval.

  Definition contains intv intv' :=
    match intv', intv with
    | _, Inan => true
    | Inan, _ => false
    | Ilbnd l', Ilbnd l => Qle_bool l l'
    | Ilbnd l', Iubnd _ => true
    | Ilbnd l', Ibnd l _ => Qle_bool l l'
    | Iubnd _, Ilbnd _ => false
    | Iubnd u', Iubnd u => Qle_bool u' u
    | Iubnd u', Ibnd _ u => Qle_bool u' u
    | Ibnd l' _, Ilbnd l => Qle_bool l l'
    | Ibnd _ u', Iubnd u => Qle_bool u' u
    | Ibnd l' u', Ibnd l u => (Qle_bool l l') && (Qle_bool u' u)
    end.

  Inductive step := 
    | ICP0 (e: list interval)								(* an infeasible e *)
    | ICP1 (e: list interval) (e': list interval) 		      			(* e' \subseteq e *) 
    | ICP2 (e: list interval) (e1: list interval) (e2: list interval). 	(* e \subseteq e1 \cup e2 *)

  Definition check_icp0 (e: list interval) := true.
  
  Fixpoint check_icp1 (e e': list interval) :=
    match e, e' with
    | nil, nil => true
    | intv::t, intv'::t' => (contains intv intv') && (check_icp1 t t')
    | _, _ => false
    end.

  Fixpoint check_icp2 (e e1 e2: list interval)  :=
    match e, e1, e2 with
    | nil, nil, nil => true
    | intv::t, intv1::t1, intv2::t2 => (contains intv intv1) && (contains intv intv2) && (check_icp2 t t1 t2)
    | _, _, _ => false
    end.
    
  Definition step_checker (st:step) :=	
    match st with
    | ICP0 e => check_icp0 e
    | ICP1 e e' => check_icp1 e e'
    | ICP2 e e1 e2 => check_icp2 e e1 e2
    end.

End Icp_Checker.
