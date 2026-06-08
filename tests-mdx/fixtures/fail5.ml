(* camlp5o *)
(* test_hashcons.ml *)

type term =
    Ref of int
  | Abs of term
  | App of term * term
[@@deriving hashcons { hashconsed_module_name = LAMH
                     ; normal_module_name = AST
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
