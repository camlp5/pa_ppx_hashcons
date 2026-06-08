(**pp -syntax camlp5o -package pa_ppx_hashcons *)

type t = M.t = A of int option
[@@deriving hashcons { hashconsed_module_name = ASTH
                     ; normal_module_name = AST
                     ; memo = {
                         memo_one = [%typ: int option]
                       }
                     }]
