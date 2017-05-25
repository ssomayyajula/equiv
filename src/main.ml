open Core.Std
open Frenetic_Decide_Spec
open Frenetic_Decide_SpecDeriv
open Frenetic_Decide_Ast
open Frenetic_Decide_Util
open Frenetic_Decide_Deriv
open Frenetic_Decide_FA
open Frenetic_Decide_Packet_Repr
open Frenetic_Decide_Enum

(*
f: Sel * Pk -> PacketSet.t option
(tau, p) -> None
(a f, p) -> Some (all p' s.t. p[f]=p'[f])

f^{-1}: PacketSet.t option -> SelPkSet.t
None -> {(tau, p) | p : packet}
Some set -> (a f, all ) where f are all common fields
*)

module SelPkSet = Set.Make (struct
  type t = packet * Spec.sel with sexp, compare
end)

module L = Label(SelPkSet)

let inv : PacketSet.t option -> SelPkSet.t = failwith ""

module E = Expand(PacketSet)

let simulates
  (type q1t)
  (type q2t)
  (type s)
  (module E : ENUM with type t = s)
  (module D1 : DFA with type q = q1t and type s = s and type x = bool)
  (module D2 : DFA with type q = q2t and type s = s and type x = bool) : bool =
    let module R = Set.Make(struct type t = D1.q * D2.q with sexp, compare end) in
    let rec helper (q1 : D1.q) (q2 : D2.q) (r : R.t ref) : bool =
      (* q1 R q2 *)
      R.mem (!r) (q1, q2) ||
      (* e1 q1 <= e2 q2 *)
      (not (D1.epsilon q1) || D2.epsilon q2) ||
      (* forall s in Sigma, helper (delta1 (q1, s)) (delta2 (q2, s)) *)
      (r := R.add (!r) (q1, q2);
       E.forall (fun s -> helper (D1.delta q1 s) (D2.delta q2 s) r)) in
    helper D1.q0 D2.q0 (ref R.empty)

let check_satisfaction s t1 t2 =
  (* Simulation check requires both terms to have the same domains *)
  FieldMap.equal ValueSet.equal (domain t1) (domain t2) && begin
  (* Build the automata for s, t1, and t2 *)
  let aut_s  = from_spec_deriv (module NaiveDeriv) s in
  let aut_t1 = dfa_of_nfa (from_netkat_deriv (module BDDDeriv) (TermSet.singleton t1)) in
  let aut_t2 = dfa_of_nfa (from_netkat_deriv (module BDDDeriv) (TermSet.singleton t2)) in
  
  (* Take the product of the NetKAT terms with the spec *)
  let prod_t1_s = product aut_t1 aut_s  in
  let prod_t2_s = product aut_t2 aut_s  in

  (* Convert the (pk, sel) labels to sets of packets or epsilon transitions (semantically) *)
  let lbl_1 = L.label inv prod_t1_s in
  let lbl_2 = L.label inv prod_t2_s in

  (* Get rid of epsilon moves and expand set transitions to packet transitions *)
  let lbl_e1 = E.expand (dfa_of_nfa (close lbl_1)) in
  let lbl_e2 = E.expand (dfa_of_nfa (close lbl_2)) in
  let module P = PacketEnum (struct let term = t1 end) in
  (*simulates (module P) lbl_e2 lbl_e1*)
  failwith "NYI"
end

let () = ()

