module Util = Frenetic_Decide_Util

module Predicate : sig
  type t =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of t * t
    | And of t * t
    | Not of t

  val to_string: t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val values : t -> Util.UnivMap.t
end

