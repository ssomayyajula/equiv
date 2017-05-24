open Core.Std
open Frenetic_Decide_Spec
open Frenetic_Decide_SpecDeriv
open Frenetic_Decide_Ast
open Frenetic_Decide_Deriv
open Frenetic_Decide_FA
open Frenetic_Decide_Packet_Repr

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

let check_satisfaction s t1 t2 =
  (* Build the automata for s, t1, and t2 *)
  let aut_s     = from_spec_deriv (module NaiveDeriv) s in
  let aut_t1    = dfa_of_nfa (from_netkat_deriv (module BDDDeriv) t1) in
  let aut_t2    = dfa_of_nfa (from_netkat_deriv (module BDDDeriv) t2) in
  
  (* Take the product of the NetKAT terms with the spec *)
  let prod_t1_s = product aut_t1 aut_s  in
  let prod_t2_s = product aut_t2 aut_s  in

  (* Convert the (pk, sel) labels to sets of packets or epsilon transitions (semantically) *)
  let lbl_e1    = L.label inv prod_t1_s in
  let lbl_e2    = L.label inv prod_t2_s in

  (* Get rid of epsilon moves and expand set transitions to packet transitions *)
  let lbl_1 = E.expand (dfa_of_nfa (close lbl_e1)) in
  let lbl_2 = E.expand (dfa_of_nfa (close lbl_e2)) in
  
  (*
  let sim = intersect (dfa_of_nfa ex_1) (complement (dfa_of_nfa ex_2)) in
  let module A = (val sim : DFA with type s = PacketSet.t option and type x = bool)
  let a = (module struct
    type x = 
  end : SyntacticDFA with type x = bool) in*)
  failwith ""

type ab = A | B

let d =
  dfa_of_nfa
    (module struct
       type q = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 with sexp, compare
       
       type s = ab
       
       type x = bool
       
       module StateSet = Set.Make (struct
         type t = q with sexp, compare
       end)
       
       let q0 = Q0
       
       let delta q s =
         let open StateSet in
         match q, s with
         | Q0, A -> of_list [Q1; Q2]
         | Q0, B -> empty
         | Q1, A -> empty
         | Q1, B -> singleton Q3
         | Q2, A -> empty
         | Q2, B -> singleton Q5
         | Q3, A -> singleton Q4
         | Q3, B -> empty
         | Q4, A -> empty
         | Q4, B -> singleton Q1
         | Q5, A -> singleton Q2
         | Q5, B -> empty
       
       let epsilon = function
         (Q1 | Q2) -> true
         | _       -> false
     end : NFA with type s = ab and type x = bool)

let main = run (complement d) [A; B; A; B; A; A]

let () = Printf.printf "%b\n" main

