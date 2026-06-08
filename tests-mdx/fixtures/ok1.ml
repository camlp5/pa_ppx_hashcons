(**pp -syntax camlp5o -package pa_ppx_hashcons *)
[@@@ocaml.warning "-11"]

let preeq_list f l1 l2 =
  List.length l1 = List.length l2 &&
  List.for_all2 f l1 l2

let prehash_list f l =
  Hashtbl.hash (List.map f l)
let hash_list = prehash_list


type t = A of int * int | B of t list
[@@deriving hashcons { hashconsed_module_name = ASTH
                     }]
