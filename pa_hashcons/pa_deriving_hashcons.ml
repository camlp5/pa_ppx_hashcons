(**pp -syntax camlp5r *)
(* pa_deriving_migrate.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;
open Surveil ;
open Pa_deriving_base ;
open Pa_ppx_utils ;

value debug = Pa_passthru.debug ;

value canon_expr e = Reloc.expr (fun _ -> Ploc.dummy) 0 e ;
value canon_ctyp ty = Reloc.ctyp (fun _ -> Ploc.dummy) 0 ty ;
value builtin_types =
  let loc = Ploc.dummy in
  List.map canon_ctyp [
    <:ctyp< string >>
  ; <:ctyp< int >>
  ; <:ctyp< int32 >>
  ; <:ctyp< int64 >>
  ; <:ctyp< nativeint >>
  ; <:ctyp< float >>
  ; <:ctyp< bool >>
  ; <:ctyp< char >>
  ]
;

open Pa_ppx_params_runtime.Runtime ;

module HC = struct

type external_funs_t = {
  preeq : expr
; prehash : expr
} [@@deriving params;]
;

type pertype_customization_t = {
  hashcons_module : option uident
; hashcons_constructor : option lident
} [@@deriving params;]
;

value extract_memo_type_list type_decls t =
  let rec rec_memo_type = fun [
    <:ctyp< $lid:lid$ >> when List.mem_assoc lid type_decls -> True
  | <:ctyp< ( $list:l$ ) >> -> List.for_all rec_memo_type l
  | _ -> False
  ] in
  let rec prim_type = fun [
    z when List.mem (canon_ctyp z) builtin_types -> True
  | <:ctyp< ( $list:l$ ) >> when List.for_all prim_type l -> True
  | _ -> False
  ] in
  let memoizable t = rec_memo_type t || prim_type t in
  match t with [
    z when prim_type z -> Left [(False, z)]
  | <:ctyp< $lid:_$ >> as z when rec_memo_type z -> Left [(True, z)]
  | <:ctyp< ( $t1$ * $t2$ ) >> when rec_memo_type t1 && rec_memo_type t2 -> Left [(True, t1);(True, t2)]

  | <:ctyp< ( $list:l$ ) >> when List.for_all memoizable l ->
    Right (List.map (fun z -> (rec_memo_type z, z)) l)

  | _ -> Ploc.raise (loc_of_ctyp t)
           (Failure Fmt.(str "extract_memo_type_list: not memoizable type:@ %a"
                           Pp_MLast.pp_ctyp t))
  ]
;

value compute_memo type_decls raw_memo =
  List.map (fun (k, v) ->
             (k, extract_memo_type_list type_decls v))
    raw_memo
;

type t = {
  optional : bool [@default False;]
; plugin_name : string
; hashconsed_module_name : uident
; normal_module_name : option uident
; type_decls : list (string * MLast.type_decl) [@computed type_decls;]
; raw_memo : (alist lident ctyp) [@default [];] [@name memo;]
; memo : (alist lident  (choice (list (bool * MLast.ctyp)) (list (bool * MLast.ctyp)))) [@computed compute_memo type_decls raw_memo;]
; external_types : (alist longid_lident external_funs_t) [@default [];]
; skip_types : (list lident) [@default [];]
; pertype_customization : (alist lident pertype_customization_t) [@default [];]
} [@@deriving params {
    formal_args = {
      t = [ type_decls ]
    }
  };]
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let optarg =
    let l = List.map (fun (k, e) -> (<:patt< $lid:k$ >>, e)) (Ctxt.options ctxt) in
    <:expr< { $list:l$ } >> in
  params type_decls optarg
;

value strip_hashcons_attributes td =
  { (td) with
    tdAttributes =
      vala_map (Std.filter (fun a ->
          not (List.mem (a |> uv |> fst |> uv |> snd)
            ["deriving"])))
        td.tdAttributes }
;

value hashcons_module_name rc name =
  match List.assoc name rc.pertype_customization with [
    {hashcons_module = Some n} -> n
  | _ -> "HC_"^name
  | exception Not_found -> "HC_"^name
  ]
;

value hashcons_constructor_names rc name =
  match List.assoc name rc.pertype_customization with [
    {hashcons_constructor = Some n} -> ["make_"^name ; n]
  | _ -> ["make_"^name]
  | exception Not_found -> ["make_"^name]
  ]
;

value generate_eq_expression loc eq_prefix ctxt rc rho ty =
  let rec prerec = fun [

    <:ctyp< $lid:lid$ >> when AList.mem ~{cmp=Reloc.eq_longid_lident} (None, <:vala< lid >>) rc.external_types ->
    (AList.assoc ~{cmp=Reloc.eq_longid_lident} (None, <:vala< lid >>) rc.external_types).preeq

  | <:ctyp< $longid:li$ . $lid:lid$ >> when AList.mem ~{cmp=Reloc.eq_longid_lident} (Some <:vala< li >>, <:vala< lid >>) rc.external_types ->
    (AList.assoc ~{cmp=Reloc.eq_longid_lident} (Some <:vala< li >>, <:vala< lid >>) rc.external_types).preeq

  | <:ctyp< $longid:li$ . $lid:lid$ >> ->
    let eq_name = eq_prefix^"_"^lid in
    Expr.prepend_longident li <:expr< $lid:eq_name$ >>

  | <:ctyp< $_$ == $t$ >> -> prerec t

  | <:ctyp:< $lid:lid$ >> as z when List.mem_assoc lid rc.type_decls ->
    let eq_name = eq_prefix^"_"^lid in
    <:expr< $lid:eq_name$ >>

  | z when List.mem (canon_ctyp z) builtin_types ->
    <:expr< (fun (x : $z$) (y : $z$) -> x = y) >>

  | <:ctyp:< $lid:lid$ >> ->
    let eq_name = eq_prefix^"_"^lid in
    <:expr< $lid:eq_name$ >>

  | <:ctyp:< $_$ $_$ >> as z ->
    let (ty, args) = Ctyp.unapplist z in
    Expr.applist <:expr< $prerec ty$ >> (List.map prerec args)

  | <:ctyp< ' $id$ >> when List.mem_assoc id rho ->
    <:expr< $lid:List.assoc id rho$ >>
  | <:ctyp:< ( $list:l$ ) >> ->
    let xpatt_ypatt_subeqs =
      List.mapi (fun i ty ->
          let x = Printf.sprintf "x_%d" i in
          let y = Printf.sprintf "y_%d" i in
          (<:patt< $lid:x$ >>,
           <:patt< $lid:y$ >>,
           <:expr< $prerec ty$ $lid:x$ $lid:y$ >>)) l in
    let xpatt (x, _, _) = x in
    let ypatt (_, x, _) = x in
    let subeq (_, _, x) = x in
    let rhs = List.fold_right (fun x e -> <:expr< $subeq x$ && $e$ >>) xpatt_ypatt_subeqs <:expr< True >> in
    <:expr< (fun ( $list:List.map xpatt xpatt_ypatt_subeqs$ )
            ( $list:List.map ypatt xpatt_ypatt_subeqs$ ) -> $rhs$) >>

  | <:ctyp:< { $list:ltl$ } >> ->
    let xlpatt_ylpatt_subeqs =
      List.mapi (fun i (_, id, _, ty, _) ->
          let x = Printf.sprintf "x_%d" i in
          let y = Printf.sprintf "y_%d" i in
          ((<:patt< $lid:id$ >>, <:patt< $lid:x$ >>),
           (<:patt< $lid:id$ >>, <:patt< $lid:y$ >>),
           <:expr< $prerec ty$ $lid:x$ $lid:y$ >>)) ltl in
    let xlpatt (x, _, _) = x in
    let ylpatt (_, x, _) = x in
    let subeq (_, _, x) = x in
    let rhs = List.fold_right (fun x e -> <:expr< $subeq x$ &&  $e$ >>) xlpatt_ylpatt_subeqs <:expr< True >> in
    <:expr< (fun { $list:List.map xlpatt xlpatt_ylpatt_subeqs$ }
            { $list:List.map ylpatt xlpatt_ylpatt_subeqs$ } -> $rhs$) >>

  | <:ctyp:< [ $list:l$ ] >> ->
    let case_branches =
      List.map (fun [
          <:constructor< $uid:ci$ of $list:tl$ >> ->
          let xpatt_ypatt_subeqs =
            List.mapi (fun i ty ->
                let x = Printf.sprintf "x_%d" i in
                let y = Printf.sprintf "y_%d" i in
                (<:patt< $lid:x$ >>,
                 <:patt< $lid:y$ >>,
                 <:expr< $prerec ty$ $lid:x$ $lid:y$ >>)) tl in
          let xpatt (x, _, _) = x in
          let ypatt (_, x, _) = x in
          let subeq (_, _, x) = x in
          let xconspat = Patt.applist <:patt< $uid:ci$ >> (List.map xpatt xpatt_ypatt_subeqs) in
          let yconspat = Patt.applist <:patt< $uid:ci$ >> (List.map ypatt xpatt_ypatt_subeqs) in
          let rhs = List.fold_right (fun x e -> <:expr< $subeq x$ && $e$ >>) xpatt_ypatt_subeqs <:expr< True >> in
          (<:patt< ($xconspat$, $yconspat$) >>, <:vala< None >>, rhs)
        ]) l in
    let case_branches = case_branches @ [
      (<:patt< _ >>, <:vala< None >>, <:expr< False >>)
    ] in
    <:expr< (fun x y -> match (x,y) with [ $list:case_branches$ ] ) >>

  | z -> Ploc.raise loc (Failure Fmt.(str "generate_eq_expression:@ unhandled type %a"
                                        Pp_MLast.pp_ctyp z))

  ] in
  prerec ty
;

value make_rho loc name td =
  let tyvars = td.tdPrm |> uv in
  List.mapi (fun i -> fun [
      (<:vala< None >>, _) ->
      Ploc.raise loc (Failure Fmt.(str "make_rho: %s: formal type-vars must all be named"
                                     name))
    | (<:vala< Some id >>, _) -> (id, Printf.sprintf "sub_%d" i)
    ]) tyvars
;

value abstract_function_body loc typemaker rho fbody =
  let args = List.map (fun (id, fname) ->
    let argty = typemaker <:ctyp< $lid:id$ >> in
    <:patt< ( $lid:fname$ : $argty$)>>) rho in
  let typeargs = List.map (fun (id, _) ->
      <:patt< (type $lid:id$) >>) rho in
  Expr.abstract_over (typeargs@args) fbody
;

value create_function_type loc typemaker rho name =
  if rho = [] then
    typemaker <:ctyp< $lid:name$ >>
  else
    let typevars = List.map (fun (id, _) -> <:ctyp< ' $id$ >>) rho in
    let thety = Ctyp.applist <:ctyp< $lid:name$ >> typevars in
    let argtypes = List.map typemaker typevars in
    let rhsty = Ctyp.arrows_list loc argtypes (typemaker thety) in
    <:ctyp< ! $list:List.map fst rho$ . $rhsty$ >>
;

value generate_preeq_bindings ctxt rc (name, td) =
  let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let node_type_name = name^"_node" in
  let rho = make_rho loc name td in
  let body = generate_eq_expression loc "preeq" ctxt rc rho td.tdDef in
  let body = match body with [
    <:expr< fun [ $list:_$ ] >> -> body
  | _ -> <:expr< fun x y -> $body$ x y >>
  ] in
  let make_type cty = <:ctyp< $cty$ -> $cty$ -> bool >> in
  let fbody = abstract_function_body loc make_type rho body in
  let ftype = create_function_type loc make_type rho node_type_name in
  let node_eq_fname = "preeq_"^node_type_name in
  let node_binding = (<:patt< ( $lid:node_eq_fname$ : $ftype$ ) >>, fbody, <:vala< [] >>) in
  let it_binding =
    let body =
      if rho <> [] || List.mem name rc.skip_types then
        let fexp = Expr.applist <:expr< $lid:node_eq_fname$ >> (List.map (fun (_, f) -> <:expr< $lid:f$ >>) rho) in
        <:expr< fun x y -> $fexp$ x y >>
      else
        <:expr< fun x y -> x == y >> in
    let fbody = abstract_function_body loc make_type rho body in
    let ftype = create_function_type loc make_type rho name in
    let eq_fname = "preeq_"^name in
    (<:patt< ( $lid:eq_fname$ : $ftype$ ) >>, fbody, <:vala< [] >>) in
  [node_binding ; it_binding]
;

value generate_hash_expression loc hash_prefix ctxt rc rho ty =
  let rec prerec = fun [

    <:ctyp< $lid:lid$ >> when AList.mem ~{cmp=Reloc.eq_longid_lident} (None, <:vala< lid >>) rc.external_types ->
    (AList.assoc ~{cmp=Reloc.eq_longid_lident} (None, <:vala< lid >>) rc.external_types).prehash

  | <:ctyp< $longid:li$ . $lid:lid$ >> when AList.mem ~{cmp=Reloc.eq_longid_lident} (Some <:vala< li >>, <:vala< lid >>) rc.external_types ->
    (AList.assoc ~{cmp=Reloc.eq_longid_lident} (Some <:vala< li >>, <:vala< lid >>) rc.external_types).prehash

  | <:ctyp< $_$ == $t$ >> -> prerec t

  | <:ctyp< $longid:li$ . $lid:lid$ >> ->
    let hash_name = hash_prefix^"_"^lid in
    Expr.prepend_longident li <:expr< $lid:hash_name$ >>

  | <:ctyp:< $lid:lid$ >> as z when List.mem_assoc lid rc.type_decls ->
    let hash_name = hash_prefix^"_"^lid in
    <:expr< $lid:hash_name$ >>

  | z when List.mem (canon_ctyp z) builtin_types ->
    <:expr< (fun (x : $z$) -> Hashtbl.hash x) >>

  | <:ctyp:< $lid:lid$ >> ->
    let hash_name = hash_prefix^"_"^lid in
    <:expr< $lid:hash_name$ >>

  | <:ctyp:< $_$ $_$ >> as z ->
    let (ty, args) = Ctyp.unapplist z in
    Expr.applist <:expr< $prerec ty$ >> (List.map prerec args)
  | <:ctyp< ' $id$ >> when List.mem_assoc id rho ->
    <:expr< $lid:List.assoc id rho$ >>
  | <:ctyp:< ( $list:l$ ) >> ->
    let xpatt_subhashs =
      List.mapi (fun i ty ->
          let x = Printf.sprintf "x_%d" i in
          (<:patt< $lid:x$ >>,
           <:expr< $prerec ty$ $lid:x$ >>)) l in
    let xpatt (x, _) = x in
    let subhash (_, x) = x in
    let rhs = List.fold_right (fun x e -> <:expr< 19 * $subhash x$ + $e$ >>) xpatt_subhashs <:expr< 0 >> in
    <:expr< (fun ( $list:List.map xpatt xpatt_subhashs$ ) -> $rhs$) >>

  | <:ctyp:< { $list:ltl$ } >> ->
    let xlpatt_subhashs =
      List.mapi (fun i (_, id, _, ty, _) ->
          let x = Printf.sprintf "x_%d" i in
          ((<:patt< $lid:id$ >>, <:patt< $lid:x$ >>),
           <:expr< $prerec ty$ $lid:x$ >>)) ltl in
    let xlpatt (x, _) = x in
    let subhash (_, x) = x in
    let rhs = List.fold_right (fun x e -> <:expr< 19 * $subhash x$ + $e$ >>) xlpatt_subhashs <:expr< 0 >> in
    <:expr< (fun { $list:List.map xlpatt xlpatt_subhashs$ } -> $rhs$) >>

  | <:ctyp:< [ $list:l$ ] >> ->
    let case_branches =
      List.mapi (fun pos -> fun [
          <:constructor< $uid:ci$ of $list:tl$ >> ->
          let xpatt_subhashs =
            List.mapi (fun i ty ->
                let x = Printf.sprintf "x_%d" i in
                (<:patt< $lid:x$ >>,
                 <:expr< $prerec ty$ $lid:x$ >>)) tl in
          let xpatt (x, _) = x in
          let subhash (_, x) = x in
          let xconspat = Patt.applist <:patt< $uid:ci$ >> (List.map xpatt xpatt_subhashs) in
          let rhs = List.fold_right (fun x e -> <:expr< 19 * $subhash x$ + $e$ >>) xpatt_subhashs <:expr< $int:string_of_int pos$ >> in
          (<:patt< $xconspat$ >>, <:vala< None >>, rhs)
        ]) l in
    <:expr< fun [ $list:case_branches$ ] >>

  | z -> Ploc.raise loc (Failure Fmt.(str "generate_hash_expression:@ unhandled type %a"
                                        Pp_MLast.pp_ctyp z))

  ] in
  prerec ty
;

value generate_prehash_bindings ctxt rc (name, td) =
  let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let node_type_name = name^"_node" in
  let rho = make_rho loc name td in
  let body = generate_hash_expression loc "prehash" ctxt rc rho td.tdDef in
  let body = match body with [
    <:expr< fun [ $list:_$ ] >> -> body
  | _ -> <:expr< fun x -> $body$ x >>
  ] in
  let make_type cty = <:ctyp< $cty$ -> int >> in
  let fbody = abstract_function_body loc make_type rho body in
  let ftype = create_function_type loc make_type rho node_type_name in
  let node_hash_fname = "prehash_"^node_type_name in
  let node_binding = (<:patt< ( $lid:node_hash_fname$ : $ftype$ ) >>, fbody, <:vala< [] >>) in
  let it_binding =
    let body =
      if rho <> [] || List.mem name rc.skip_types then
        let fexp = Expr.applist <:expr< $lid:node_hash_fname$ >> (List.map (fun (_, f) -> <:expr< $lid:f$ >>) rho) in
        <:expr< fun x -> $fexp$ x >>
      else
        <:expr< fun x -> x.hkey >> in
    let fbody = abstract_function_body loc make_type rho body in
    let ftype = create_function_type loc make_type rho name in
    let hash_fname = "prehash_"^name in
    (<:patt< ( $lid:hash_fname$ : $ftype$ ) >>, fbody, <:vala< [] >>) in
  [node_binding ; it_binding]

;

value generate_hash_bindings ctxt rc (name, td) =
  let loc = loc_of_type_decl td in
  let rho = make_rho loc name td in
  let node_rhs = generate_hash_expression loc "hash" ctxt rc rho td.tdDef in
  let node_rhs = match node_rhs with [
    <:expr< fun [ $list:_$ ] >> -> node_rhs
  | _ -> <:expr< fun x -> $node_rhs$ x >>
  ] in
  let node_type_name = name^"_node" in
  let node_hash_fname = "hash_"^node_type_name in
  let make_type cty = <:ctyp< $cty$ -> int >> in
  let node_rhs = abstract_function_body loc make_type rho node_rhs in
  let node_ftype = create_function_type loc make_type rho node_type_name in

  let node_binding = (<:patt< ( $lid:node_hash_fname$ : $node_ftype$ ) >>, node_rhs, <:vala< [] >>) in

  let it_binding =
    let body =
      if rho <> [] || List.mem name rc.skip_types then
        let fexp = Expr.applist <:expr< $lid:node_hash_fname$ >> (List.map (fun (_, f) -> <:expr< $lid:f$ >>) rho) in
        <:expr< fun x -> $fexp$ x >>
      else
        <:expr< fun x -> x.hkey >> in
    let fbody = abstract_function_body loc make_type rho body in
    let ftype = create_function_type loc make_type rho name in
    let hash_fname = "hash_"^name in
    (<:patt< ( $lid:hash_fname$ : $ftype$ ) >>, fbody, <:vala< [] >>) in

  [node_binding; it_binding]
;

value ctyp_make_tuple loc l =
  match l with [
    [] -> Ploc.raise loc (Failure "ctyp_make_tuple: invalid empty-list arg")
  | [t] -> t
  | l -> <:ctyp< ( $list:l$ ) >>
  ]
;

value expr_make_tuple loc l =
  match l with [
    [] -> Ploc.raise loc (Failure "expr_make_tuple: invalid empty-list arg")
  | [t] -> t
  | l -> <:expr< ( $list:l$ ) >>
  ]
;

value patt_make_tuple loc l =
  match l with [
    [] -> Ploc.raise loc (Failure "patt_make_tuple: invalid empty-list arg")
  | [t] -> t
  | l -> <:patt< ( $list:l$ ) >>
  ]
;

value find_matching_memo loc rc l =
  let eq_lists l1 l2 =
    List.length l1 = List.length l2 &&
    List.for_all2 (fun (b1, t1) (b2, t2) ->
        b1=b2 && Reloc.eq_ctyp t1 t2) l1 l2 in
  match List.find_map (fun (memo, t) ->
    match t with [
      Right l' when eq_lists l l' -> Some memo
    | Left l' when eq_lists l l' -> Some memo
    | _ -> None
    ]) rc.memo with [
    Some n -> n
  | None ->
    let ty = <:ctyp< ( $list:List.map snd l$ ) >> in
    Ploc.raise loc (Failure Fmt.(str "find_matching_memo: no match:@ Please declare a memoizer of type <<%s>>@."
                                   (Eprinter.apply Pcaml.pr_ctyp Pprintf.empty_pc ty)))
  ]
;

value to_expr loc (v, (_, _)) = <:expr< $lid:v$ >> ;
value to_patt loc (v, (_, _)) = <:patt< $lid:v$ >> ;
value to_typatt loc (v, (_, ty)) = <:patt< ( $lid:v$ : $ty$ ) >> ;
value to_ctyp (_, (_, ty)) = ty ;

value generate_memo_item_with_deps loc ctxt rc (memo_fname, memo_tys) =
  match memo_tys with [

    Left l when not (List.exists fst l) ->
    let vars_types = List.mapi (fun i x -> (Printf.sprintf "v_%d" i, x)) l in
    let vars_exps = List.map (to_expr loc) vars_types in
    let vars_patts = List.map (to_patt loc) vars_types in
    let z = ctyp_make_tuple loc (List.map to_ctyp vars_types) in
    let vars_tuple = expr_make_tuple loc vars_exps in
    let mname = Printf.sprintf "HT_%s" memo_fname in
    let recompute_expr = Expr.applist <:expr< f >> vars_exps in
    let body = <:expr<
          try $uid:mname$.find ht $vars_tuple$
          with [ Not_found -> do {
            let newv = $recompute_expr$ in 
            $uid:mname$.add ht $vars_tuple$ newv ;
            newv
          }]
      >> in
    let fun_body = Expr.abstract_over vars_patts body in
    let fun_type = 
      let f = Ctyp.arrows_list loc (List.map to_ctyp vars_types) <:ctyp< 'a >> in
      <:ctyp< ! 'a . $f$ -> $uid:mname$.t 'a -> $f$ >> in
    (memo_fname,
     (<:str_item<
      declare
        module $uid:mname$ = Hashtbl.Make(struct
          type t = $z$ ;
          value equal = $generate_eq_expression loc "eq" ctxt rc [] z$ ;
          value hash = $generate_hash_expression loc "hash" ctxt rc [] z$ ;
        end) ;
      value $lid:memo_fname$ = fun f ht ->
        $fun_body$
        ;
      end
     >>, []))

  | Left [(True, z)] ->
    let mname = Printf.sprintf "HT_%s" memo_fname in
    let memo_ftype =
      let f = <:ctyp< $z$ -> 'a >> in
      <:ctyp< ! 'a . $f$ -> $f$ >> in
    (memo_fname,
     (<:str_item<
      declare
        module $uid:mname$ = Ephemeron.K1.Make(struct
          type t = $z$ ;
          value equal = $generate_eq_expression loc "preeq" ctxt rc [] z$ ;
          value hash = $generate_hash_expression loc "prehash" ctxt rc [] z$ ;
        end) ;
      value $lid:memo_fname$ = fun f ->
        let ht = $uid:mname$.create 251 in
        fun ( x : $z$ ) ->
          try $uid:mname$.find ht x
          with [ Not_found -> do {
            let newv = f x in 
            $uid:mname$.add ht x newv ;
            newv
          }]
        ;
      end
     >>, []))

  | Left [(True, z0); (True, z1)] ->
    let mname = Printf.sprintf "HT_%s" memo_fname in
    let memo_ftype =
      let f = <:ctyp< $z0$ -> $z1$ -> 'a >> in
      <:ctyp< ! 'a . $f$ -> $f$ >> in
    (memo_fname,
     (<:str_item<
      declare
        module $uid:mname$ = Ephemeron.K2.Make
          (struct
            type t = $z0$ ;
            value equal = $generate_eq_expression loc "preeq" ctxt rc [] z0$ ;
            value hash = $generate_hash_expression loc "prehash" ctxt rc [] z0$ ;
           end)
          (struct
            type t = $z1$ ;
            value equal = $generate_eq_expression loc "preeq" ctxt rc [] z1$ ;
            value hash = $generate_hash_expression loc "prehash" ctxt rc [] z1$ ;
           end) ;
      value $lid:memo_fname$ = fun f ->
        let ht = $uid:mname$.create 251 in
        fun ( x : $z0$ ) ( y : $z1$ ) ->
          try $uid:mname$.find ht (x,y)
          with [ Not_found -> do {
            let newv = f x y in 
            $uid:mname$.add ht (x,y) newv ;
            newv
          }]
        ;
      end
     >>, []))

  | Right l ->
    let vars_types = List.mapi (fun i x -> (Printf.sprintf "v_%d" i, x)) l in
    let (hc_args, prim_args) = filter_split (fun (_, (x, _)) -> x) vars_types in do {
      assert (hc_args <> []) ;
      if prim_args <> [] then
        let hc_memo_name = find_matching_memo loc rc (List.map snd hc_args) in
        let prim_tupletype = ctyp_make_tuple loc (List.map to_ctyp prim_args) in
        let prim_memo_name = find_matching_memo loc rc [(False, prim_tupletype)] in
        let prim_mname = "HT_"^prim_memo_name in

        let hc_function_expr =
          Expr.abstract_over (List.map (to_typatt loc) hc_args)
            <:expr< $uid:prim_mname$.create 251 >> in

        let f_call = Expr.applist <:expr< f >> (List.map (to_expr loc) vars_types) in
        let prim_function_expr =
          Expr.abstract_over [(patt_make_tuple loc (List.map (to_typatt loc) prim_args))] f_call in
        
        let prim_memo_call =
          Expr.applist <:expr< $lid:prim_memo_name$ >>
            [prim_function_expr; <:expr< ht >> ; expr_make_tuple loc (List.map (to_expr loc) prim_args)] in
                               
        let hc_call = Expr.applist <:expr< hc_f >> (List.map (to_expr loc) hc_args) in

        let fun_body =
          Expr.abstract_over (List.map (to_typatt loc) vars_types)
          <:expr< let ht = $hc_call$ in
                  $prim_memo_call$ >> in

        let fun_type = Ctyp.arrows_list loc (List.map to_ctyp vars_types) <:ctyp< 'a >> in
        let memo_ftype =  <:ctyp< ! 'a . $fun_type$ -> $fun_type$ >> in
        let mono_fun_type = monomorphize_ctyp fun_type in
        (memo_fname,
         (<:str_item<
          value $lid:memo_fname$ = fun (type a) -> fun ( f : $mono_fun_type$ ) ->
            let hc_f = $lid:hc_memo_name$ $hc_function_expr$ in
            $fun_body$
        >>, [hc_memo_name; prim_memo_name]))
      else match hc_args with [
        []|[_]|[_;_] -> assert False
        | [arg1; arg2 :: rest] ->
        let first_memo_name = find_matching_memo loc rc (List.map snd [arg1;arg2]) in
        let pairty = <:ctyp< ( $list:[to_ctyp arg1;to_ctyp arg2]$ ) >> in
        let second_memo_name = find_matching_memo loc rc [(True, pairty) :: List.map snd rest] in

        let second_f_call =
          Expr.applist <:expr< second_f p >> (List.map (to_expr loc) rest) in
        let body = <:expr<
                     let p = first_f $to_expr loc arg1$ $to_expr loc arg2$ in
                     $second_f_call$ >> in
        let fun_body =
          Expr.abstract_over (List.map (to_typatt loc) hc_args) body in
        
        let f_call = Expr.applist <:expr< f >> (List.map (to_expr loc) hc_args) in
        let second_f_function =
          Expr.abstract_over [<:patt< ($to_patt loc arg1$, $to_patt loc arg2$) >> :: List.map (to_patt loc) rest]
            f_call in
        (memo_fname,
         (<:str_item<
          value $lid:memo_fname$ f =
            let first_f = $lid:first_memo_name$ (fun a1 a2 -> (a1, a2)) in
            let second_f = $lid:second_memo_name$
              $second_f_function$ in
            $fun_body$
          >>, [first_memo_name; second_memo_name]))
      ]
    }

  ]
;


value space = Fmt.(const string " ") ;

value tsort_memos memos =
  let open Tsort in
  let unsorted = List.map fst memos in
  let adj = List.map (fun (f, (_, deps)) -> (f,deps)) memos in
  let sorted = Tsort.tsort  (fun v l -> [v::l]) adj [] in do {
    if debug.val then
      Fmt.(pf stderr "[tsort memo: <<%a>> -> <<%a>>]\n%!"
             (list ~{sep=space} string) unsorted
             (list ~{sep=space} string) sorted)
    else () ;
    sorted
    |> List.map (fun n -> match List.assoc n memos with [ x -> [fst x] | exception Not_found -> [] ])
    |> List.concat
  }
;


value generate_memo_items loc arg rc =
  let items = List.map (generate_memo_item_with_deps loc arg rc) rc.memo in
  let items = tsort_memos items in
  items
;

value flatten_str_items sil =
  let rec flatrec = fun [
    <:str_item< declare $list:l$ end >> ->
    List.concat (List.map flatrec l)
  | si -> [si]
  ]
  in List.concat (List.map flatrec sil)
;

value separate_bindings l =
  let (ml, vl)  = List.fold_left (fun (mb,vb) -> fun [
      <:str_item< module $uid:_$ = $_$ >> as z -> ([ z :: mb ], vb)
    | <:str_item< value $list:l$ >> -> (mb, l @ vb)
    ]) ([], []) l in
  (List.rev ml, List.rev vl)
;

value generate_hashcons_module ctxt rc (name, td) =
  let loc = loc_of_type_decl td in
  if uv td.tdPrm <> [] || List.mem name rc.skip_types then <:str_item< declare end >> else
  let modname = hashcons_module_name rc name in
  let node_name = name^"_node" in
  let preeq_name = "preeq_"^name^"_node" in
  let prehash_name = "prehash_"^name^"_node" in
  <:str_item< module $uid:modname$ = Hashcons.Make(struct
              type t = $lid:node_name$ ;
              value equal = $lid:preeq_name$ ;
              value hash = $lid:prehash_name$ ;
              end) >>
;

value generate_hashcons_constructor ctxt rc (name, td) =
  let loc = loc_of_type_decl td in
  if uv td.tdPrm <> [] || List.mem name rc.skip_types then <:str_item< declare end >> else
  let modname = hashcons_module_name rc name in
  let consnames = hashcons_constructor_names rc name in
  let htname = name^"_ht" in
  let bindings = List.map (fun n ->
      (<:patt< $lid:n$ >>, <:expr< fun x -> $uid:modname$.hashcons $lid:htname$ x >>, <:vala< [] >>))
      consnames in
  <:str_item< declare
                 value $lid:htname$ = $uid:modname$.create 10007 ;
                 value $list:bindings$ ;
              end >>
;

end
;

value make_twolevel_type_decl ctxt rc ~{with_manifest} ~{skip_hashcons} td =
  let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let data_name = name^"_node" in
  let tyargs =
    let tyvars = td.tdPrm |> uv in
    List.map (fun [
        (<:vala< None >>, _) ->
        Ploc.raise loc (Failure Fmt.(str "hashconsed_type_decl: %s: formal type-vars must all be named"
                                       name))
      | (<:vala< Some id >>, _) -> <:ctyp< ' $id$ >>
      ]) tyvars in
  let hc_tdDef =
    let data_type = <:ctyp< $lid:data_name$ >> in
    if skip_hashcons then
      Ctyp.applist data_type tyargs
    else
      <:ctyp< hash_consed $Ctyp.applist data_type tyargs$ >> in
  [ { (td) with
      tdNam =
        let n = <:vala< data_name >> in
        <:vala< (loc, n) >>
      ; tdDef = match td.tdDef with [
          <:ctyp< $_$ == $t$ >> when not with_manifest -> t
        | <:ctyp< $_$ == $t$ >> when with_manifest -> td.tdDef
        | t when is_generative_type t && with_manifest ->
          Ploc.raise (loc_of_type_decl td)
            (Failure Fmt.(str "cannot generate requested \"normal\" type declaration b/c original type is not manifest: %s"
                            name))
        | t -> t
        ]
    }
  ; <:type_decl< $lid:name$ $_list:td.tdPrm$ = $hc_tdDef$ >>
  ]
;

value normal_type_decl ctxt rc td =
  let skip_hashcons = True in
  let with_manifest = True in
  make_twolevel_type_decl ctxt rc ~{with_manifest=with_manifest} ~{skip_hashcons=skip_hashcons} td
;

value hashconsed_type_decl ctxt rc td =
  let name = td.tdNam |> uv |> snd |> uv in
  let skip_hashcons = uv td.tdPrm <> [] || List.mem name rc.HC.skip_types in
  let with_manifest = False in
  make_twolevel_type_decl ctxt rc ~{with_manifest=with_manifest} ~{skip_hashcons=skip_hashcons} td
;

value str_item_gen_hashcons name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = HC.build_context loc arg tdl in
    let new_tdl =
      tdl
      |> List.map (hashconsed_type_decl arg rc)
      |> List.concat
      |> List.map HC.strip_hashcons_attributes in
    let normal_module = match rc.normal_module_name with [
      None -> <:str_item< declare end >>
    | Some normname ->
      let normal_tdl =
        tdl
        |> List.map (normal_type_decl arg rc)
        |> List.concat
        |> List.map HC.strip_hashcons_attributes in
      <:str_item< module $uid:normname$ = struct
                  type $list:normal_tdl$ ;
                  end
                  >>
    ] in
    let preeq_bindings = List.concat (List.map (HC.generate_preeq_bindings arg rc) rc.HC.type_decls) in
    let prehash_bindings = List.concat (List.map (HC.generate_prehash_bindings arg rc) rc.HC.type_decls) in
    let hashcons_modules = List.map (HC.generate_hashcons_module arg rc) rc.HC.type_decls in
    let hash_bindings = List.concat (List.map (HC.generate_hash_bindings arg rc) rc.HC.type_decls) in
    let hashcons_constructors = List.map (HC.generate_hashcons_constructor arg rc) rc.HC.type_decls in
    let memo_items = HC.generate_memo_items loc arg rc in
    let memo_items = List.map (Reloc.str_item (fun _ -> Ploc.dummy) 0) memo_items in
    let memo_items = HC.flatten_str_items memo_items in
    let full_memo_item = <:str_item< declare $list:memo_items$ end >> in
      <:str_item< declare
                  $stri:normal_module$ ;
                  module $uid:rc.hashconsed_module_name$ = struct
                  open Hashcons ;
                  type $list:new_tdl$ ;
                  value rec $list:preeq_bindings$ ;
                  value rec $list:prehash_bindings$ ;
                  declare $list:hashcons_modules @ hashcons_constructors$ end ;
                  value rec $list:hash_bindings$ ;
                  $stri:full_memo_item$ ;
                  end ;
                end>>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "hashcons"
; alternates = []
; options = ["optional"
            ; "hashconsed_module_name"; "normal_module_name"; "memo"
            ; "external_types"; "skip_types"; "pertype_customization"]
; default_options = []
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_hashcons
; sig_item = (fun arg e -> assert False)
})
;

