open Core
open Frenetic_Decide_Ast
open Frenetic_Decide_Util

type 'domain_witness hyperpoint = int list
type 'domain_witness codepoint = int
type 'domain_witness index = { i : int }
type 'domain_witness index0 = { i : int }

module type Domain = sig
  val domain : ValueSet.t FieldMap.t
end

type pk = packet

module Packet = struct
  type field = Field.t
  
  type value = Value.t

  open Core.Std
  
  let test (f : field) (v : value) (pk : pk) : bool =
    Option.value_map (Map.find pk f) ~f:(Value.equal v) ~default:false

  let modify (f : field) (v : value) (pk : pk) : pk =
    Map.add pk ~key:f ~data:v  
end

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

module Make(D : Domain) : S = struct

  let domain : (Field.t * Value.t list) list =
    Core.Std.Map.to_alist (Core.Std.Map.map D.domain ~f:ValueSet.elements)

  type domain_witness

  module Hyperpoint = struct
    type t = domain_witness hyperpoint

    let dimension =
      Core.Std.List.map domain ~f:(fun (_,vs) -> List.length vs + 1)

    let injection : (Field.t * (Value.t option -> int)) list =
      Core.Std.List.Assoc.map domain ~f:(fun vs ->
        Core.Std.List.mapi vs ~f:(fun i v -> (v, i+1))
        |> Core.Std.Int.Map.of_alist_exn
        |> (fun m -> function
            | None -> 0
            | Some v -> Core.Std.Option.value (Core.Std.Map.find m v) ~default:0))

    let ejection : (Field.t * (int -> Value.t option)) list =
      Core.Std.List.Assoc.map domain ~f:Core.Std.List.to_array
      |> Core.Std.List.Assoc.map ~f:(fun inj v -> if v = 0 then None else Some inj.(v-1))

    open Core.Std
    
    let to_codepoint t =
      List.fold2_exn t dimension ~init:0 ~f:(fun cp v n -> v + n * cp)

    let of_codepoint cp =
      List.fold_right dimension ~init:(cp,[]) ~f:(fun n (cp, hp) ->
        let (cp, v) = Int.(cp /% n, cp % n) in
        (cp, v::hp))
      |> snd

    let to_pk t =
      List.fold2_exn t ejection ~init:FieldMap.empty ~f:(fun pk v (f, vej) ->
        Option.value_map (vej v)
          ~f:(fun data -> FieldMap.add pk ~key:f ~data)
          ~default:pk)

    let of_pk pk =
      List.map injection ~f:(fun (f, vinj) -> vinj (FieldMap.find pk f))
  end

  module Codepoint = struct
    type t = domain_witness codepoint
    let to_hyperpoint = Hyperpoint.of_codepoint
    let of_hyperpoint = Hyperpoint.to_codepoint
    let to_pk = Fn.compose Hyperpoint.to_pk to_hyperpoint
    let of_pk = Fn.compose of_hyperpoint Hyperpoint.of_pk
    let max = (Core.Std.List.fold ~init:1 ~f:( * ) Hyperpoint.dimension) - 1
    let to_index cp : domain_witness index = { i = cp + 1  }
    let of_index (idx : domain_witness index) = idx.i - 1
    let to_index0 cp : domain_witness index0 = { i = cp }
    let of_index0 (idx : domain_witness index0) = idx.i 
  end

  module Index = struct
    type t = domain_witness index
    let of_pk = Fn.compose Codepoint.to_index Codepoint.of_pk
    let to_pk = Fn.compose Codepoint.to_pk Codepoint.of_index
    let max = Codepoint.(to_index max)
    let test f n t = Packet.test f n (to_pk t)
    let modify f n t = of_pk (Packet.modify f n (to_pk t))
    let test' f n i = test f n { i = i }
    let modify' f n i = (modify f n { i = i }).i
  end

  module Index0 = struct
    type t = domain_witness index0
    let of_pk = Fn.compose Codepoint.to_index0 Codepoint.of_pk
    let to_pk = Fn.compose Codepoint.to_pk Codepoint.of_index0
    let max = Codepoint.(to_index0 max)
    let test f n t = Packet.test f n (to_pk t)
    let modify f n t = of_pk (Packet.modify f n (to_pk t))
    let test' f n i = test f n { i = i }
    let modify' f n i = (modify f n { i = i }).i
  end

end
