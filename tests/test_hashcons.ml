(* camlp5o *)
(* test_hashcons.ml *)

type 'a id = 'a [@@hashcons_skip]
and 'a option = 'a Option.t = None | Some of 'a
and term_id = term id
and term_option = term option
and term =
    Ref of int
  | Abs of term_id
  | App of term * term
  | Foo of term_option
[@@hashcons_module Term][@@hashcons_constructor term]
[@@deriving hashcons { module_name = LAM
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo_int = [%typ: int]
                       ; memo_int2 = [%typ: int * int]
                       ; memo_int_int_int_int = [%typ: int * int * int * int]
                       ; memo_term_int = [%typ: term * int]
                       ; memo_int_term = [%typ: int * term]
                       ; memo_term_term = [%typ: term * term]
                       ; memo_term2_term = [%typ: (term * term) * term]
                       ; memo_term_term_term = [%typ: term * term * term]
                       ; memo_term2_term2 = [%typ: (term * term) * term * term]
                       ; memo_term4 = [%typ: term * term * term * term]
                       ; memo_term4_int2 = [%typ: term * int * term * term * int * term]
                       ; memo_term3_term = [%typ: ((term * term) * term) * term]
                       }
                     }]
;;

let preeq_option f x y = match (x,y) with
    (None, None) -> true
  | (Some x, Some y) -> f x y
  | _ -> false
let prehash_option f x =
  Hashtbl.hash (Option.map f x)
let hash_option = prehash_option

module XX = struct

type term_option = term option
and term =
    Ref of int
  | Abs of term
  | App of term * term
  | Foo of term_option
[@@hashcons_module Term][@@hashcons_constructor term]
[@@deriving hashcons { module_name = LAM2
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo_int_term = [%typ: int * term]
                       ; memo_int = [%typ: int]
                       }
                     }]
end
;;


type variable = int (* 1..max_var *) ;;
let preeq_variable x y = x = y ;;
let prehash_variable x = Hashtbl.hash x ;;
let hash_variable = prehash_variable

type bdd = Zero | One | Node of variable * bdd (*low*) * bdd (*high*)
[@@deriving hashcons { module_name = BDD
                     ; memo = {
                         memo_bdd = [%typ: bdd]
                       ; memo_bdd_bdd = [%typ: bdd * bdd]
                       }
                     }]
;;

let preeq_list f l1 l2 =
  List.length l1 = List.length l2 &&
  List.for_all2 f l1 l2

let prehash_list f l =
  Hashtbl.hash (List.map f l)
let hash_list = prehash_list

module Ploc = struct
include Ploc

let prehash_t x = Hashtbl.hash x
let hash_t = prehash_t
let preeq_t x y = x = y
end

[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@add [%%import: 'a Ploc.vala]]
    [@with Ploc.vala := vala]
]
[@@deriving hashcons { module_name = AST
                     ; memo = {
                         memo_expr = [%typ: expr]
                       }
                     }]
;;
