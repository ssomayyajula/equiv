open Core.Std
open Frenetic_Decide_Spec
open Frenetic_Decide_SpecDeriv
open Frenetic_Decide_Ast
open Frenetic_Decide_Deriv
open Frenetic_Decide_FA

module SelSet = Set.Make (struct
  type t = Spec.sel option with sexp, compare
end)

module L = Label(SelSet)

let check_satisfaction s t1 t2 =
  let aut_s     = from_spec_deriv   (module NaiveDeriv) s  in
  let aut_t1    = from_netkat_deriv (module BDDDeriv)   t1 in
  let aut_t2    = from_netkat_deriv (module BDDDeriv)   t2 in
  let prod_t1_s = product aut_t1 aut_s                     in
  let prod_t2_s = product aut_t2 aut_s                     in
  let f : packet -> SelSet.t = failwith ""                          in
  let ex_1      = L.label f prod_t1_s          in
  let ex_2      = L.label f prod_t2_s          in
  (*empty (intersect (dfa_of_nfa ex_1) (complement (dfa_of_nfa ex_2)))
*)
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

