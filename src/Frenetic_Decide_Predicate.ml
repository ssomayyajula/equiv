module Ast = Frenetic_Decide_Ast
module Util = Frenetic_Decide_Util

module Predicate = struct
  type t =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of t * t
    | And of t * t
    | Not of t with compare

  let rec to_string pred =
    match pred with
    | One         -> "1"
    | Zero        -> "0"
    | Test (f, v) ->
        let fs = Util.Field.to_string f in
        let vs = Util.Value.to_string v in
        Printf.sprintf "(%s = %s)" fs vs
    | Or   (a, b) -> Printf.sprintf "(%s ∨ %s)" (to_string a) (to_string b)
    | And  (a, b) -> Printf.sprintf "(%s ∧ %s)" (to_string a) (to_string b)
    | Not   a     -> Printf.sprintf "¬(%s)" (to_string a)

  let rec compile (pred: t) : Ast.Term.t =
    match pred with
    | One         -> Ast.Term.one
    | Zero        -> Ast.Term.zero
    | Test (f, v) -> Ast.Term.test f v
    | Or   (a, b) -> Ast.Term.plus (Ast.TermSet.of_list [compile a; compile b])
    | And  (a, b) -> Ast.Term.times [compile a; compile b]
    | Not   a     -> Ast.Term.not (compile a)
  
  let equal b1 b2 = Ast.Term.equal (compile b1) (compile b2)
  
  let values b = Ast.Term.values (compile b)
end

