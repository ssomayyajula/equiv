open Core.Std
open Frenetic_Decide_Ast
open Frenetic_Decide_Deriv
open Frenetic_Decide_Spec
open Frenetic_Decide_SpecDeriv

module type FA = sig
  (* The type of states and sets of states *)
  type q with sexp, compare

  (* The alphabet of this NFA *)
  type s

  (* The type of observations *)
  type x
  
  (* The initial state of this NFA *)
  val q0 : q

  (* The observation map *)
  val epsilon : q -> x
end

module type DFA = sig
  include FA
  val delta : q -> s -> q
end

val complement : (module DFA with type x = bool) -> (module DFA)

val empty : (module DFA) -> bool

val join : ('x -> 'x -> 'x) 
        -> (module DFA with type x = 'x)
        -> (module DFA with type x = 'x)
        -> (module DFA)
 
val product : (module DFA with type x = bool)
           -> (module DFA with type x = bool)
           -> (module DFA)

val intersect : (module DFA with type s = 's and type x = bool)
             -> (module DFA with type s = 's and type x = bool)
             -> (module DFA)

module type NFA = sig
  include FA
  module StateSet : sig
    include Set.S with type Elt.t = q
  end
  val delta : q -> s -> StateSet.t
end

module Label : functor (S : Set.S) -> sig
  val label : ('s2 -> S.t) -> (module DFA with type s = S.Elt.t) -> (module NFA with type s = 's2)
end

val nfa_of_dfa : (module DFA) -> (module NFA)

val dfa_of_nfa : (module NFA with type x = bool) -> (module DFA)

val from_netkat_deriv : (module DerivTerm) -> Term.t -> (module DFA)

val from_spec_deriv : (module SpecDeriv) -> Spec.t -> (module DFA)

