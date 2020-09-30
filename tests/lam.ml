(* camlp5o *)
(* lam.ml *)


type term =
    Ref of int
  | Abs of term
  | App of term * term
