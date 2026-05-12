(* camlp5o *)
(* lam.ml *)


type term =
    Ref of int
  | Abs of term
#if OCAML_VERSION >= (5,4,0)
  | App of (f:term * term)
#else
  | App of (term * term)
#endif
