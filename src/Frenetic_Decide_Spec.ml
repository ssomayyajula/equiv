open Core.Std
open Sexplib.Conv
open Frenetic_Decide_Util
open Frenetic_Decide_Predicate
open Frenetic_Decide_Ast

module FieldSet = Set.Make(Field)

(*
type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

module PacketSet = Set.Make (struct
  type t = packet with sexp, compare
end)

let packet_to_string pkt = Printf.sprintf "[%s]"
    (String.concat ~sep:";"
       (FieldMap.fold pkt ~init:[]
          ~f:(fun ~key ~data acc -> (Printf.sprintf "%s := %s" (Field.to_string key) (Value.to_string data) :: acc))))

let point_to_string (pkt1, pkt2) = Printf.sprintf "(%s,%s)" (packet_to_string pkt1) (packet_to_string pkt2)
*)

module rec SpecBase : sig
  type sel =
    | Alpha of FieldSet.t
    | Beta of Predicate.t
    | Tau with compare

  type t = spec Hashcons.hash_consed and
  spec =
    | Sel of sel
    | Plus of SpecSetBase.t
    | Times of t list
    | Star of t
    | Zero
    | One

  type int_spec =
    | ISel of sel
    | IPlus of Int.Set.t
    | ITimes of int list
    | IStar of int
    | IZero
    | IOne with compare

  val project: spec -> (t -> int) -> int_spec
  val comparable_of_spec: spec -> int_spec
  val hashable_of_spec: spec -> int_spec

  val sel_equal : sel -> sel -> bool
  val compare: t -> t -> int
  val compare_sel : sel -> sel -> int
  val compare_spec: spec -> spec -> int
  val sexp_of_t: t -> Sexplib.Sexp.t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_spec: spec -> Sexplib.Sexp.t
  val spec_of_sexp: Sexplib.Sexp.t -> spec
  val sel_of_sexp : Sexplib.Sexp.t -> sel
  val sexp_of_sel : sel -> Sexplib.Sexp.t
end = struct
  type sel =
    | Alpha of FieldSet.t
    | Beta of Predicate.t
    | Tau with compare
  
  (*
  let compare_sel s1 s2 =
    match s1, s2 with
    | Alpha f1, Alpha f2 -> FieldSet.compare f1 f2
    | Beta b1, Beta b2 -> 
    | Tau, Tau -> 0
    | _ -> *)
  
  type t = spec Hashcons.hash_consed and
  spec =
    | Sel of sel
    | Plus of SpecSetBase.t
    | Times of t list
    | Star of t
    | Zero
    | One

  type int_spec =
    | ISel of sel
    | IPlus of Int.Set.t
    | ITimes of int list
    | IStar of int
    | IZero
    | IOne with compare

  let project t f =
    match t with
    | Sel s -> ISel s
    | Plus ts -> IPlus (Int.Set.map ts ~f)
    | Times ts -> ITimes (List.map ts ~f)
    | Star t -> IStar (f t)
    | Zero -> IZero
    | One -> IOne
  let comparable_of_spec spec = project spec (fun t -> t.Hashcons.tag)
  let hashable_of_spec spec = project spec (fun t -> t.Hashcons.hkey)

  let compare_spec a b =
    let a_comp = (comparable_of_spec a) in
    let b_comp = (comparable_of_spec b) in
    compare_int_spec a_comp b_comp

  let compare_sel s1 s2 = compare_spec (Sel s1) (Sel s2)

  let sel_equal s1 s2 =
    match s1, s2 with
    | Alpha f1, Alpha f2 -> FieldSet.equal f1 f2
    | Beta b1, Beta b2   -> Predicate.equal b1 b2
    | Tau, Tau           -> true
    | _                  -> false
  
  let compare a b = compare_spec a.Hashcons.node b.Hashcons.node

  let sexp_of_t t = failwith "TODO"
  let t_of_sexp s = failwith "TODO"
  let sexp_of_spec t = failwith "TODO"
  let spec_of_sexp s = failwith "TODO"

  let sel_of_sexp s = match spec_of_sexp s with Sel sel -> sel | _ -> failwith "sel_of_sexp error"
  let sexp_of_sel s = sexp_of_spec (Sel s)
end
and SpecSetBase : sig
  include Set.S with type Elt.t = SpecBase.t
end = Set.Make (struct
  type t = SpecBase.t
  let compare = SpecBase.compare
  let t_of_sexp = SpecBase.t_of_sexp
  let sexp_of_t = SpecBase.sexp_of_t
end)

module Spec = struct
  include SpecBase
(*
  let rec eval (t : TermBase.t) (pkt : packet) = match t.Hashcons.node with
    | Assg (f,v) -> PacketSet.singleton (FieldMap.add pkt ~key:f ~data:v)
    | Test (f,v) -> begin match FieldMap.find pkt f with
        | Some v' -> if v' = v
          then PacketSet.singleton pkt
          else PacketSet.empty
        | None -> PacketSet.empty
      end
    | Dup -> raise (Failure "t must be dup-free")
    | Plus ts -> TermSetBase.fold ts ~f:(fun acc t -> PacketSet.union (eval t pkt) acc) ~init:PacketSet.empty
    | Times ts -> List.fold ts ~init:(PacketSet.singleton pkt) ~f:(fun accum t ->
        PacketSet.fold accum ~init:PacketSet.empty ~f:(fun acc pkt -> PacketSet.union acc (eval t pkt)))
    | Not t -> let ret = eval t pkt in
      begin
        match PacketSet.length ret with
        | 0 -> PacketSet.singleton pkt
        | 1 -> PacketSet.empty
        | _ -> raise (Failure "Negation of a non-predicate")
      end
    (* TODO: Copy fixpoint code from Frenetic *)
    | Star t -> raise (Failure "NYI")
    | Zero -> PacketSet.empty
    | One -> PacketSet.singleton pkt

  let compare_ab t point =
    let input,output = point in
    PacketSet.exists (eval t input) ~f:(FieldMap.equal Value.equal output)
*)
  let rec to_string (t : t) : string =
    let out_precedence (t : t) : int =
      match t.Hashcons.node with
        | Plus _ -> 0
        | Times _ -> 1
        | Star _ -> 2
        | _ -> 3 in
    let protect (u:t) : string =
      let s = to_string u in
      if out_precedence t <= out_precedence u then s
      else Printf.sprintf "(%s)" s in
    let assoc_to_string (op : string) (init : string) (s : string list) : string =
      match s with
        | [] -> init
        | _ -> String.concat ~sep:op s in
    match t.Hashcons.node with
      | Sel s ->
        Printf.sprintf "sel"
      | Plus (ts) ->
        assoc_to_string " + " "drop"
          (List.map ~f:protect (SpecSetBase.elements ts))
      | Times (ts) ->
        assoc_to_string ";" "id" (List.map ~f:protect ts)
      | Star (t) ->
        (protect t) ^ "*"
      | Zero ->
        "drop"
      | One ->
        "id"

  (* module H = Make(struct *)
      (* type t = TermBase.term with sexp, compare *)
      (* let equal a b = compare a b = 0 *)
      (* let hash = Hashtbl.hash *)
    (* end) *)

  module H = Hashcons.Make(struct
      type t = SpecBase.spec

      let equal a b =
        match a, b with
        | Sel Tau, Sel Tau -> true
        | Sel (Alpha f1), Sel (Alpha f2) -> FieldSet.equal f1 f2
        | Sel (Beta b1), Sel (Beta b2) -> Predicate.equal b1 b2
        | Plus t1, Plus t2 -> SpecSetBase.equal t1 t2
        | Times t1, Times t2 ->
            List.length t1 = List.length t2 && List.for_all2_exn t1 t2 phys_equal
        | Star t1, Star t2 -> phys_equal t1 t2
        | Zero, Zero -> true
        | One, One -> true
        | _ -> false

      let hash t = Hashtbl.hash (hashable_of_spec t)
    end)

  let hashtbl = H.create 100
  let beta b = H.hashcons hashtbl (Sel (Beta b))
  let alpha f = H.hashcons hashtbl (Sel (Alpha f))
  let tau = H.hashcons hashtbl (Sel Tau)
  let plus ts = H.hashcons hashtbl (Plus ts)
  let times ts = H.hashcons hashtbl (Times (List.fold_right ts ~init:[] ~f:(fun x acc -> match x.Hashcons.node with
      | One -> acc
      | Times ts' -> ts' @ acc
      | _ -> x :: acc)))
  let star t = H.hashcons hashtbl (Star t)
  let zero = H.hashcons hashtbl Zero
  let one = H.hashcons hashtbl One

  module UnivMap = SetMapF(Field)(Value)
  (* Collect the possible values of each variable *)
  let values (t : SpecBase.t) : UnivMap.t =
    let rec collect (m : UnivMap.t) (t : SpecBase.t) : UnivMap.t =
      match t.Hashcons.node with
	| Sel (Beta b) -> Predicate.values b
	| Plus s -> SpecSetBase.fold s ~init:m ~f:collect
	| Times s -> List.fold_right s ~init:m ~f:(fun a b -> collect b a)
	| Star x -> collect m x
	| (Sel _  | Zero  | One ) -> m in
    collect UnivMap.empty t

  let equal t1 t2 = compare t1 t2 = 0
  let rec size t =
    match t.Hashcons.node with
      | Sel _ ->
        1
      | Plus ts ->
        SpecSetBase.fold ts
          ~f:(fun n ti -> (size ti) + n)
          ~init:1
      | Times ts ->
        List.fold_left ts
          ~f:(fun n ti -> n + (size ti))
          ~init:1
      | Star t -> 1 + size t
      | Zero -> 1
      | One -> 1
end

module SpecSet = struct
  include SpecSetBase
  let to_string ts = Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map (elements ts) Spec.to_string))
end

