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

let (_ : int LAM.option) = (Some 1) ;;
(Ref 1) = (Ref 1) ;;

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
