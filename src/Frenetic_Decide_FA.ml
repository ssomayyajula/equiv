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

module type NFA = sig
  include FA
  module StateSet : sig
    include Set.S with type Elt.t = q
  end
  val delta : q -> s -> StateSet.t
end

let join (type x')
         f
         (module A1 : DFA with type x = x')
         (module A2 : DFA with type x = x') =
  (module struct
     type q = A1.q * A2.q with sexp, compare
     
     type s = A1.s * A2.s

     type x = x'

     let q0 = A1.q0, A2.q0
     
     let delta (q1, q2) (s1, s2) = A1.delta q1 s1, A2.delta q2 s2
     
     let epsilon (q1, q2) = f (A1.epsilon q1) (A2.epsilon q2)
   end : DFA)

let product = join (&&)

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
   end : DFA)

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

let dfa_of_nfa (module N : NFA with type x = bool) =
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
   end : DFA)

let complement (module D : DFA with type x = bool) =
  (module struct
     type q = D.q with sexp, compare
     
     type s = D.s
     
     type x = D.x
     
     let q0 = D.q0
     
     let delta = D.delta
     
     let epsilon = Fn.non D.epsilon
   end : DFA)

let empty (module D : DFA) = failwith ""

module Label (S : Set.S) = struct
  let label (type s2) (f : s2 -> S.t) (module D : DFA with type s = S.Elt.t) =
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
     end : NFA with type s = s2)
end

(* Both these definitions use Antimirov's Theorem 4.1 in his seminal paper *)

let from_netkat_deriv (module D : DerivTerm) t  = failwith ""
(*  let open D in
  (module struct
     type q = TermSet.t with sexp, compare

     type s  = point

     type x = bool

     let q0 = t

     let delta q s = DMatrix.run (get_d (make_term (TermSet.singleton q))) s

     let epsilon q = EMatrix.run (get_e (make_term (TermSet.singleton q)))
   end : DFA)*)

let from_spec_deriv (module S : SpecDeriv) sp =
  let open S in
  (module struct
     type q = Spec.t with sexp, compare
     
     type s = Spec.sel

     type x = bool     

     let q0 = sp
     
     let delta q s = D.run (get_d (make q)) s
     
     let epsilon q = E.run (get_e (make q))
   end : DFA)

