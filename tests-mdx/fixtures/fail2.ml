(**pp -syntax camlp5o -package pa_ppx_hashcons *)

type t = A of int * int
[@@deriving hashcons { hashconsed_module_name = ASTH
                     ; normal_module_name = AST
                     }]
