exception Empty

open Core.Std
open Frenetic_Decide_Util
open Frenetic_Decide_Ast
open Frenetic_Decide_Spec
open Frenetic_Decide_Deriv

module type SpecDeriv = sig

  type t with sexp

  module E : sig
    type t with sexp
    (*val fold : t -> init:'a -> f:('a -> point -> 'a) -> 'a*)
    val run : t -> bool
    (*val compare : t -> t -> int
    val empty : t
    val intersection_empty : t -> t -> bool
    val union : t -> t -> t*)
  end

  module D : sig
    type t with sexp
    val run : t -> Spec.sel -> Spec.t
    (*val compare : t -> t -> int
    val equivalent : (SpecSet.t -> SpecSet.t -> bool) -> t -> t -> bool
    val points : t -> EMatrix.t*)
  end

  val make : Spec.t -> t
  val get_e : t -> E.t
  val get_d : t -> D.t
  (*val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
  val to_string : t -> string*)
end

module Product : functor (N : DerivTerm) -> functor (S : SpecDeriv) -> sig
  type t with sexp

  module E : sig
    type t with sexp

    val run : t -> point -> bool
  end

  module D : sig
    type t with sexp
    
    val run : t -> point -> Spec.sel -> TermSet.t * Spec.t
  end
  
  val make : Term.t -> Spec.t -> t
  val get_e : t -> E.t
  val get_d : t -> D.t
end

module NaiveDeriv : SpecDeriv
