open Frenetic_Decide_Util
open Frenetic_Decide_Ast

type 'domain_witness hyperpoint = private int list
type 'domain_witness codepoint = private int
type 'domain_witness index = private { i : int }
type 'domain_witness index0 = { i : int }

module type Domain = sig
  val domain : ValueSet.t FieldMap.t
end

type pk = packet

module type S = sig
  type domain_witness

  (** Encoding of packet in n dimensional space.
      More precisely, a packet is encoded as a point in a hypercube, with the
      coordinates being of type int.
      If [dimension] = {k1, ..., kn}, then the hypercube is given by
        {0, ..., k1} x ... x {0, ..., kn}.
      The points within this cube are represented as lists, rather than tuples,
      because n is not known at compile time.
  *)
  module rec Hyperpoint : sig
    type t = domain_witness hyperpoint
    val dimension : int list
    val to_codepoint : t -> Codepoint.t
    val of_codepoint : Codepoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
  end

  (** Encoding of packets as integers >= 0, i.e. points in single dimensional space. *)
  and Codepoint : sig
    type t = domain_witness codepoint
    val max : t
    val to_hyperpoint : t -> Hyperpoint.t
    val of_hyperpoint : Hyperpoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
    val to_index : t -> Index.t
    val of_index : Index.t -> t
    val to_index0 : t -> Index0.t
    val of_index0 : Index0.t -> t
  end

  (** Encoding of packets as strictly positive integers, i.e. matrix indices. *)
  and Index : sig
    type t = domain_witness index
    val max : t
    val of_pk : pk -> t
    val to_pk : t -> pk
    val test : Field.t -> Value.t -> t -> bool
    val modify : Field.t -> Value.t -> t -> t
    val test' : Field.t -> Value.t -> int -> bool
    val modify' : Field.t -> Value.t -> int -> int
  end

  (** Encoding of packets as positive integers (including 0), i.e. matrix indices. *)
  and Index0 : sig
    type t = domain_witness index0
    val max : t
    val of_pk : pk -> t
    val to_pk : t -> pk
    val test : Field.t -> Value.t -> t -> bool
    val modify : Field.t -> Value.t -> t -> t
    val test' : Field.t -> Value.t -> int -> bool
    val modify' : Field.t -> Value.t -> int -> int
  end
end


module Make(D : Domain) : S

