
module Inject = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Lam_hashcons.LAM.term]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = []
    ; dispatchers = {
        migrate_term_node = {
          srctype = [%typ: term_node]
        ; dsttype = [%typ: Lam_hashcons.LAMH.term_node]
        }
      ; migrate_term = {
          srctype = [%typ: term]
        ; dsttype = [%typ: Lam_hashcons.LAMH.term]
        ; code = (fun __dt__ x ->
            Lam_hashcons.LAMH.term (__dt__.migrate_term_node __dt__ x)
          )
        }
      }
    }
]
end

module Project = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Lam_hashcons.LAMH.term]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = []
    ; dispatchers = {
        migrate_term_node = {
          srctype = [%typ: term_node]
        ; dsttype = [%typ: Lam_hashcons.LAM.term]
        }
      ; migrate_term = {
          srctype = [%typ: term]
        ; dsttype = [%typ: Lam_hashcons.LAM.term]
        ; code = (fun __dt__ x ->
            __dt__.migrate_term_node __dt__ x.Hashcons.node
          )
        }
      }
    }
]
end
