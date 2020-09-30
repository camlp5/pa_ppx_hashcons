(* camlp5o *)
(* hCLam.ml *)

[%%import: Lam.term]
[@@hashcons_module Term][@@hashcons_constructor term]
[@@deriving hashcons { module_name = HC
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo_int_term = [%typ: int * term]
                       ; memo_int = [%typ: int]
                       }
                     }]
