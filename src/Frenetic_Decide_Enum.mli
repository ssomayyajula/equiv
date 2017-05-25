open Core.Std
open Frenetic_Decide_Util
open Frenetic_Decide_Ast

module type Policy = sig 
  val term : Term.t
end

(* Characterizes finite and totally ordered types *)
module type ENUM = sig
  type t
  
  module S : Set.S with type Elt.t = t
  
  val min : t
  val max : t
  val succ : t -> t
  val pred : t -> t
  val forall : (t -> bool) -> bool
  val elements : S.t
end

val domain : Term.t -> ValueSet.t FieldMap.t

(* Packets are injective into strictly positive integers *)
(* We choose an arbitrary bound from above to make it finite *)
module PacketEnum : functor (P : Policy) -> ENUM with type t = packet

