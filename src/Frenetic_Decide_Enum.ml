open Core.Std
open Frenetic_Decide_Ast
open Frenetic_Decide_Util
open Frenetic_Decide_Packet_Repr

module type Policy = sig
  val term : Term.t
end

module type ENUM = sig 
  type t
  module S : Set.S with type Elt.t = t
  val min : t 
  val max : t 
  val succ : t -> t
  val pred : t -> t
  val forall : (t -> bool) -> bool
  val elements : unit -> S.t
end

let domain t =
    let rec domain t d = 
      let open Term in
      match t.Hashcons.node with
      | One | Zero | Dup -> d
      | Test (f,n) | Assg (f,n) ->
          FieldMap.change d f (function 
          | None -> Some (ValueSet.singleton n)
          | Some ns -> Some (ValueSet.add n ns))
      | Not p | Star p -> domain p d 
      | Plus ts -> TermSet.fold ts ~init:d ~f:(Fn.flip domain)
      | Times ts -> List.fold_right ts ~init:d ~f:domain
    in domain t FieldMap.empty

module PacketEnum (P : Policy) = struct
  type t = packet

    (*UnivMap.fold (fun k v m -> FieldMap.add m ~key:k ~data:v) (Term.values t) FieldMap.empty*)
  
  module D = Make(struct
    let domain = domain P.term
  end)
  
  module S = Set.Make (struct
    type t = packet with sexp, compare
  end)
  
  open D.Index0
  
  let min = to_pk {i = 0}
  
  let max = to_pk D.Index0.max
  
  let succ pk = to_pk {i = (of_pk pk).i + 1}
  
  let pred pk = to_pk {i = (of_pk pk).i - 1}
  
  let forall f =
    let rec helper t = f t && (if of_pk t = of_pk max then true else helper (succ t)) in
    helper min
  
  let elements () =
    let rec helper s t = if of_pk t = of_pk max then S.add s t else helper (S.add s t) (succ t) in
    helper S.empty min
end

