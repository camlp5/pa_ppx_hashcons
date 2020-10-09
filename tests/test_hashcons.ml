(* camlp5o *)
(* test_hashcons.ml *)

type 'a id = 'a
and 'a option = 'a Option.t = None | Some of 'a
and term_id = term id
and term_option = term option
and term =
    Ref of int
  | Abs of term_id
  | App of term * term
  | Foo of term_option
[@@deriving hashcons { hashconsed_module_name = LAMH
                     ; normal_module_name = LAM
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
                     ; skip_types = [
                         id
                       ]
                     ; pertype_customization = {
                         term = {
                           hashcons_module = Term
                         ; hashcons_constructor = term
                         }
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

type term =
    Ref of int
  | Abs of term
  | App of term * term
  | Foo of term Option.t
[@@deriving hashcons { hashconsed_module_name = LAM2H
                     ; normal_module_name = LAM2
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo_int_term = [%typ: int * term]
                       ; memo_int = [%typ: int]
                       }
                     ; external_types = {
                         Option.t = {
                           preeq = (fun f x y -> match (x,y) with
                               (None, None) -> true
                             | (Some x, Some y) -> f x y
                             | _ -> false)
                         ; prehash = (fun f x ->
                             Hashtbl.hash (Option.map f x))
                         }
                       }
                     ; pertype_customization = {
                         term = {
                           hashcons_module = Term
                         ; hashcons_constructor = term
                         }
                       }
                     }]
end
;;


type variable = int (* 1..max_var *) ;;
let preeq_variable x y = x = y ;;
let prehash_variable x = Hashtbl.hash x ;;
let hash_variable = prehash_variable

type bdd = Zero | One | Node of variable * bdd (*low*) * bdd (*high*)
[@@deriving hashcons { hashconsed_module_name = BDDH
                     ; normal_module_name = BDD
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

[%%import: MLast.expr
    [@with
       loc := Ploc.t ;
       type_var := MLast.type_var ;
    ]
]
[@@deriving hashcons { hashconsed_module_name = ASTH
                     ; normal_module_name = AST
                     ; memo = {
                         memo_expr = [%typ: expr]
                       }
                     ; external_types = {
                         Ploc.t = {
                           preeq = (fun x y -> x = y)
                         ; prehash = (fun x -> Hashtbl.hash x)
                         }
                       ; Ploc.vala = {
                           preeq = (fun f x y -> match (x,y) with
                               (Ploc.VaAnt s1, Ploc.VaAnt s2) -> s1=s2
                             | (Ploc.VaVal v1, Ploc.VaVal v2) -> f v1 v2
                             )
                         ; prehash = (fun f x -> match x with
                             Ploc.VaAnt s -> Hashtbl.hash s
                           | Ploc.VaVal v -> f v
                           )
                         }
                       ; MLast.type_var = {
                           preeq = (fun x y -> x = y)
                         ; prehash = (fun x -> Hashtbl.hash x)
                         }
                       }
                     ; skip_types = [
                         longid_lident
                       ; attribute
                       ; attributes_no_anti
                       ; attributes
                       ]
                     }]
;;
