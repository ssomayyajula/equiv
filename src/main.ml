open Core.Std
open Frenetic_Decide_Spec
open Frenetic_Decide_SpecDeriv
open Frenetic_Decide_Ast
open Frenetic_Decide_Util
open Frenetic_Decide_Deriv
open Frenetic_Decide_FA
open Frenetic_Decide_Enum

(*
f: Sel * Pk -> PacketSet.t option
(tau, p) -> None
(a f, p) -> Some (all p' s.t. p[f]=p'[f])

f^{-1}: PacketSet.t option -> SelPkSet.t
None -> {(tau, p) | p : packet}
Some set -> (a f, all ) where f are all common fields
*)

module PkSelSet = Set.Make (struct
  type t = packet * Spec.sel with sexp, compare
end)

module L = Label(PkSelSet)

module E = Expand(PacketSet)

(* already_seen, common := {}
   for pk in pks
     for field in pk
       if field already seen then skip
       else already_seen += field;
       if pk.field != common.field then common -= field
       else common += (field, pk.field)
   return common.keys *)

module FS = Frenetic_Decide_Spec.FieldSet

let common_fields pks =
  let common = ref FieldMap.empty in
  let discarded = ref FieldSet.empty in
  PacketSet.iter pks ~f:(FieldMap.iter
    ~f:(fun ~key:f ~data:v ->
          if      FieldSet.mem f (!discarded) then ()
          else if FieldMap.mem (!common) f then begin
            if Value.compare v (FieldMap.find_exn (!common) f) <> 0 then
              (common := FieldMap.remove (!common) f;
               discarded := FieldSet.add f (!discarded))
            else ()
          end
          else common := FieldMap.add ~key:f ~data:v (!common)));
  !common

let simulates (type s')
              (module E : ENUM with type t = s')
              (module D1 : DFA with type s = s' and type x = bool)
              (module D2 : DFA with type s = s' and type x = bool) : bool =
  let module R = Set.Make(struct type t = D1.q * D2.q with sexp, compare end) in
  (*let rec sim (q1 : D1.q) (q2 : D2.q) (r : R.t ref) : bool =
    (* q1 R q2 *)
    R.mem (!r) (q1, q2) ||
    (* e1 q1 <= e2 q2 *)
    (not (D1.epsilon q1) || D2.epsilon q2) ||
    (* forall s in Sigma, helper (delta1 (q1, s)) (delta2 (q2, s)) *)
    (r := R.add (!r) (q1, q2);
     E.forall (fun s -> sim (D1.delta q1 s) (D2.delta q2 s) r)) in
  *)
  let r        = ref R.empty in
  
  let alphabet = E.elements () in

  let module WorkList = WorkList (struct
    type t = D1.q * D2.q
    let compare (a1, b1) (a2, b2) =
      match D1.compare_q a1 a2 with
      | 0 -> D2.compare_q b1 b2
      | k -> k
  end) in

  let rec main_loop wl =
    if WorkList.is_empty wl then true
    else
      let (q1, q2) = WorkList.hd wl in
      if (not (D1.epsilon q1) || D2.epsilon q2) then false
      else
        let rest = WorkList.tl wl in
        if R.mem (!r) (q1, q2) then main_loop rest
        else begin
          r := R.add (!r) (q1, q2);
          main_loop (E.S.fold alphabet ~init:rest ~f:(fun acc s -> WorkList.add (D1.delta q1 s, D2.delta q2 s) acc))
        end in
  main_loop (WorkList.singleton (D1.q0, D2.q0))
  (*sim D1.q0 D2.q0 (ref R.empty)*)

let check_satisfaction s t1 t2 =
  (* Build the automata for s, t1, and t2 *)
  print_string "Building term and specification automata...\n";
  let aut_s  = from_spec_deriv (module NaiveDeriv) s in
  let aut_t1 = dfa_of_nfa (from_netkat_deriv (module BDDDeriv) (TermSet.singleton t1)) in
  let aut_t2 = dfa_of_nfa (from_netkat_deriv (module BDDDeriv) (TermSet.singleton t2)) in
  print_string "Building product automata...\n";
  (* Take the product of the NetKAT terms with the spec *)
  let prod_t1_s = product aut_t1 aut_s in
  let prod_t2_s = product aut_t2 aut_s in
  
  let module P = PacketEnum (struct let term = Term.plus (TermSet.of_list [t1; t2]) end) in
   
  let inv : PacketSet.t option -> PkSelSet.t =
    let open Spec in
    function
    | None -> (*PkSelSet.empty*) PkSelSet.map (P.elements ()) ~f:(fun pk -> (pk, Tau))
    | Some pks -> let fields = common_fields pks in
                  let t = Term.times (FieldMap.fold fields ~init:[] ~f:(fun ~key:f ~data:v acc -> Term.assg f v :: acc)) in
                  let module PP = PacketEnum (struct let term = t end) in
                  let sel = Alpha (FieldMap.fold fields ~init:FS.empty ~f:(fun ~key:k ~data:_ acc -> FS.add acc k)) in
                  PkSelSet.map (PP.elements ()) ~f:(fun pk -> pk, sel) in
  
  (* Convert the (pk, sel) labels to sets of packets or epsilon transitions (semantically) *)
  print_string "Resolving labels of product automata...\n";
  let lbl_1 = L.label inv prod_t1_s in
  let lbl_2 = L.label inv prod_t2_s in
  
  print_string "Expanding and closing label sets...\n";
  (* Get rid of epsilon moves and expand set transitions to packet transitions *)
  let lbl_e1 = E.expand (dfa_of_nfa (close lbl_1)) in
  let lbl_e2 = E.expand (dfa_of_nfa (close lbl_2)) in
  print_string "Computing simulation...\n";
  simulates (module P) lbl_e2 lbl_e1

let main = Printf.printf "Answer: %b\n"
  (let sw = Field.of_string "sw" in
   let pt = Field.of_string "pt" in
   let open Spec in
   let open Term in
   (check_satisfaction
      (Spec.plus (SpecSet.of_list [(alpha (FS.singleton sw)); (alpha (FS.singleton pt))]))
      (assg sw 1)
      (assg sw 2)))

