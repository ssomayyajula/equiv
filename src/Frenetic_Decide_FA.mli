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

val run : (module DFA with type s = 's and type x = 'x) -> 's list -> 'x

(* The alphabets of automata are finite but large--so
   let's not store them in memory, but iterate over them as necessary *)
module type LazyFiniteSet = sig
  type elt
  val fold_until : init:'accum
                -> f:('accum -> elt -> [ `Continue of 'accum | `Stop of 'accum ])
                -> 'accum
end

module type SyntacticDFA = sig
  include DFA
  
  (* Antimirov Thm. 3.4: FA of t has at most |t| + 1 states *)
  val size_bound : int
  
  val alphabet : (module LazyFiniteSet with type elt = s)
end

val complement : (module DFA with type s = 's and type x = bool) -> (module DFA with type s = 's and type x = bool)

val empty : (module SyntacticDFA with type x = bool) -> bool

val join : ('x -> 'x -> 'x) 
        -> (module DFA with type s = 's1 and type x = 'x)
        -> (module DFA with type s = 's2 and type x = 'x)
        -> (module DFA with type s = 's1 * 's2 and type x = 'x)
 
val product : (module DFA with type s = 's1 and type x = bool)
           -> (module DFA with type s = 's2 and type x = bool)
           -> (module DFA with type s = 's1 * 's2 and type x = bool)

val intersect : (module DFA with type s = 's and type x = bool)
             -> (module DFA with type s = 's and type x = bool)
             -> (module DFA with type s = 's and type x = bool)

module type NFA = sig
  include FA
  module StateSet : sig
    include Set.S with type Elt.t = q
  end
  val delta : q -> s -> StateSet.t
end

val close : (module NFA with type s = 's option and type x = bool) -> (module NFA with type s = 's and type x = bool)

module Label : functor (S : Set.S) -> sig
  val label : ('s2 -> S.t) -> (module DFA with type s = S.Elt.t and type x = 'x) -> (module NFA with type s = 's2 and type x = 'x)
end

val nfa_of_dfa : (module DFA) -> (module NFA)

val dfa_of_nfa : (module NFA with type s = 's and type x = bool) -> (module DFA with type s = 's and type x = bool)

val from_netkat_deriv : (module DerivTerm) -> TermSet.t -> (module NFA with type s = packet and type x = bool)

val from_spec_deriv : (module SpecDeriv) -> Spec.t -> (module DFA with type s = Spec.sel and type x = bool)

