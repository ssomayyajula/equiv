open Core.Std
open Frenetic_Decide_Util
open Frenetic_Decide_Predicate
open Frenetic_Decide_Ast

module FieldSet : sig
  include Set.S with type Elt.t = Field.t
end

(*
type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

val packet_to_string : packet -> string
val point_to_string : point -> string

module PacketSet : sig
  include Set.S with type Elt.t = packet
end
*)

module rec Spec : sig
  type sel =
    | Alpha of FieldSet.t
    | Beta of Predicate.t
    | Tau

  type t = spec Hashcons.hash_consed and
  spec =
    | Sel of sel
    | Plus of SpecSet.t
    | Times of t list
    | Star of t
    | Zero
    | One

  val sel_equal : sel -> sel -> bool
  val compare: t -> t -> int
  val compare_spec: spec -> spec -> int
  val sexp_of_t: t -> Sexplib.Sexp.t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_spec: spec -> Sexplib.Sexp.t
  val spec_of_sexp: Sexplib.Sexp.t -> spec 
  
  val alpha : FieldSet.t -> t
  val beta : Predicate.t -> t
  val tau : t
  val plus : SpecSet.t -> t
  val times : t list -> t
  val star : t -> t
  val zero : t
  val one : t

  val equal : t -> t -> bool
  (*val compare_ab : t -> point -> bool
  val eval : t -> packet -> PacketSet.t*)
  val to_string : t -> string
  val values : t -> UnivMap.t
  val size : t -> int
end and SpecSet : sig
  include Set.S with type Elt.t = Spec.t
  val to_string : t -> string
end

