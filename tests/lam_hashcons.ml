(** -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
(* hCLam.ml *)

[%%import: Lam.term]
[@@deriving hashcons { hashconsed_module_name = LAMH
                     ; normal_module_name = LAM
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo_int_term = [%typ: int * term]
                       ; memo_int = [%typ: int]
                       }
                     ; pertype_customization = {
                         term = {
                           hashcons_module = Term
                         ; hashcons_constructor = term
                         }
                       }
                     }]
