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

module Product (N : DerivTerm) (S : SpecDeriv) = struct
  module E = struct
    type t = N.EMatrix.t * S.E.t with sexp
    
    let run (ne, se) p = N.EMatrix.run ne p && S.E.run se
    
    let make e s = N.get_e (N.make_term (TermSet.singleton e)), S.get_e (S.make s)
  end
  
  module D = struct
    type t = N.DMatrix.t * S.D.t with sexp
    
    let run (nd, sd) p s = N.DMatrix.run nd p, S.D.run sd s
    
    let make e s = N.get_d (N.make_term (TermSet.singleton e)), S.get_d (S.make s)
  end
  
  type t = D.t * E.t with sexp
  
  let make e s = (D.make e s, E.make e s)
  
  let get_e = snd

  let get_d = fst
end

module NaiveDeriv = struct
  open Spec
  
  module E = struct
    type t = bool with sexp

    let run e = e
    
    let rec make t =
      match t.Hashcons.node with
        (Zero | Sel  _) -> false
      | (One  | Star _) -> true
      | Plus  es        -> SpecSet.fold es ~init:false ~f:(fun acc e -> acc || make e)
      | Times es        -> List.fold_left es ~init:true  ~f:(fun acc e -> acc && make e)
  end
  
  module D = struct
    type t = Spec.sel -> Spec.t with sexp
    
    let run d s = d s
    
    let spec_of_bool b = if b then one else zero
    
    let rec make t s =
      match t.Hashcons.node with
        (Zero | One)   -> zero
      | Sel s'         -> spec_of_bool (Spec.sel_equal s s')
      | Plus es        -> plus (SpecSet.map es ~f:(fun e -> make e s))
      | (Times [_] | Times []) -> failwith "Invalid times"
      | Times [e1; e2] -> plus (SpecSet.of_list [times [make e1 s; e2]; times [spec_of_bool (E.make e1); make e2 s]])
      | Times (h :: t) -> make (times [h; times t]) s
      | Star e         -> times [make e s; t]
  end

  type t = D.t * E.t with sexp
  
  let make s = (D.make s, E.make s)
  
  let get_e = snd
  
  let get_d = fst
end

