open Core.Std
open Frenetic_Decide_Spec
open Frenetic_Decide_SpecDeriv
open Frenetic_Decide_Deriv
open Frenetic_Decide_Ast

module type FA = sig
  type q with sexp, compare
  type s
  type x
  val q0 : q
  val epsilon : q -> x
end

module type DFA = sig
  include FA
  val delta : q -> s -> q
end

let run (type s) (type x) (module D : DFA with type s = s and type x = x) =
  Fn.compose D.epsilon (List.fold_left ~init:D.q0 ~f:D.delta)

module type NFA = sig
  include FA
  module StateSet : sig
    include Set.S with type Elt.t = q
  end
  val delta : q -> s -> StateSet.t
end

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

let join (type x')
         (f : x' -> x' -> x')
         (type s1)
         (type s2)
         (module A1 : DFA with type s = s1 and type x = x')
         (module A2 : DFA with type s = s2 and type x = x') =
  (module struct
     type q = A1.q * A2.q with sexp, compare
     
     type s = A1.s * A2.s

     type x = x'

     let q0 = A1.q0, A2.q0
     
     let delta (q1, q2) (s1, s2) = A1.delta q1 s1, A2.delta q2 s2
     
     let epsilon (q1, q2) = f (A1.epsilon q1) (A2.epsilon q2)
   end : DFA with type s = s1 * s2 and type x = x')

let product (type s1)
            (type s2)
            (module A1 : DFA with type s = s1 and type x = bool)
            (module A2 : DFA with type s = s2 and type x = bool) = join (&&) (module A1) (module A2)

let intersect (type s')
              (module A1 : DFA with type s = s' and type x = bool)
              (module A2 : DFA with type s = s' and type x = bool) =
  (module struct
     type q = A1.q * A2.q with sexp, compare
     
     type s = s'

     type x = bool

     let q0 = A1.q0, A2.q0

     let delta (q1, q2) s = A1.delta q1 s, A2.delta q2 s

     let epsilon (q1, q2) = A1.epsilon q1 && A2.epsilon q2
   end : DFA with type s = s' and type x = bool)

let nfa_of_dfa (module D : DFA) =
  (module struct
     type q = D.q with sexp, compare
     
     module StateSet = Set.Make (struct
       type t = q with sexp, compare
     end)
     
     type s = D.s
     
     type x = D.x
     
     let q0 = D.q0     
     
     let delta q s = StateSet.singleton (D.delta q s)
     
     let epsilon = D.epsilon
     
   end : NFA)

let dfa_of_nfa (type s) (module N : NFA with type s = s and type x = bool) =
  (module struct
     type q = N.StateSet.t with sexp, compare

     type s = N.s

     type x = N.x

     let q0 = N.StateSet.singleton N.q0
     
     module StateSetSet = Set.Make (struct
       type t = N.StateSet.t with sexp, compare
     end)
     
     let delta qs s =
       StateSetSet.fold (StateSetSet.map qs ~f:(fun q -> N.delta q s)) ~init:N.StateSet.empty ~f:N.StateSet.union

     let epsilon = N.StateSet.exists ~f:N.epsilon
   end : DFA with type s = s and type x = bool)

let complement (type s) (module D : DFA with type s = s and type x = bool) =
  (module struct
     type q = D.q with sexp, compare
     
     type s = D.s
     
     type x = D.x
     
     let q0 = D.q0
     
     let delta = D.delta
     
     let epsilon = Fn.non D.epsilon
   end : DFA with type s = s and type x = bool)

let empty (module D : SyntacticDFA with type x = bool) = (*failwith ""
   let rng = List.init Fn.id (D.size_bound - 1) in
   let strs = D.alphabet.fold_until ~init:true ~
   List.exists (failwith "") (run (module D))*)
   failwith ""

module Label (S : Set.S) = struct
  let label (type s2) (type x') (f : s2 -> S.t) (module D : DFA with type s = S.Elt.t and type x = x') =
    (module struct
       type q = D.q with sexp, compare
       
       module StateSet = Set.Make (struct
         type t = q with sexp, compare
       end)
       
       type s = s2
       
       type x = D.x
       
       let q0 = D.q0
       
       let delta q s' =
         S.fold (f s') ~init:StateSet.empty ~f:(fun acc s -> StateSet.add acc (D.delta q s))
       
       let epsilon = D.epsilon
     end : NFA with type s = s2 and type x = x')
end

let close (type s') (module N : NFA with type s = s' option and type x = bool) =
  (module struct
     type q = N.q with sexp, compare
     
     module StateSet = N.StateSet
     
     type s = s'
     
     type x = N.x
     
     module StateSetSet = Set.Make (struct
       type t = StateSet.t with sexp, compare
     end)
     
     let e_closure q =
       let rec helper set =
         let diff =
           StateSet.diff
             (StateSetSet.fold
                (StateSetSet.map set ~f:(Fn.flip N.delta None)) ~init:StateSet.empty ~f:StateSet.union) set in
         if StateSet.is_empty diff then
           set
         else helper (StateSet.union set diff) in
       helper (StateSet.singleton q)
     
     let q0 = N.q0
     
     let delta q s = StateSetSet.fold (StateSetSet.map (e_closure q) ~f:(Fn.flip N.delta (Some s))) ~init:StateSet.empty ~f:StateSet.union
     
     let epsilon q = StateSet.exists (e_closure q) ~f:N.epsilon
   end : NFA with type s = s' and type x = bool)

(* Steffen's magic *)
let from_netkat_deriv (module D : DerivTerm) t0 =
  (module struct
     open D
     
     type q = Start | Final | State of TermSet.t * packet with sexp, compare

     module StateSet = Set.Make (struct
       type t = q with sexp, compare
     end)

     type s = packet

     type x = bool

     let q0 = Start

     let delta q beta =
       match q with
       | Start            -> StateSet.singleton (State (t0, beta))
       | Final            -> StateSet.empty
       | State (t, alpha) ->
           let trans =
             if EMatrix.run (get_e (make_term t)) (alpha, beta) then
               StateSet.singleton Final
             else StateSet.empty in
           StateSet.add trans (State (DMatrix.run (get_d (make_term t)) (alpha, beta), beta))

     let epsilon = (=) Final
   end : NFA with type s = packet and type x = bool)

(* Antimirov Theorem 4.1 *)
let from_spec_deriv (module S : SpecDeriv) sp =
  let open S in
  (module struct
     type q = Spec.t with sexp, compare
     
     type s = Spec.sel

     type x = bool     

     let q0 = sp
     
     let delta q = D.run (get_d (make q))
     
     let epsilon q = E.run (get_e (make q))
   end : DFA with type s = Spec.sel and type x = bool)

