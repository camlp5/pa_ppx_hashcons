(* camlp5o *)
(* test_hashcons.ml *)

let preeq_option f x y = match (x,y) with
    (None, None) -> true
  | (Some x, Some y) -> f x y
  | _ -> false
let prehash_option f x =
  Hashtbl.hash (Option.map f x)
let hash_option = prehash_option

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
                             | _ -> false
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
