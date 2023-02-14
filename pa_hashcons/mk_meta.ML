#include "../version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|# Specifications for the "pa_ppx_hashcons" preprocessor:
version = "%s"
description = "pa_ppx_hashcons deriver"

  package "runtime" (
    requires = "hashcons"
  )

  requires(toploop) = "camlp5,pa_ppx.deriving_plugins.show,pa_ppx.params_runtime"
  archive(toploop) = "pa_deriving_hashcons.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving_plugins.show,pa_ppx.params_runtime,pa_ppx_hashcons.runtime"
    archive(syntax,preprocessor,-native) = "pa_deriving_hashcons.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_hashcons.cmx"

  package "link" (
  requires = "camlp5,pa_ppx.deriving_plugins.show.link,pa_ppx.params_runtime"
  archive(byte) = "pa_deriving_hashcons.cmo"
  archive(native) = "pa_deriving_hashcons.cmx"
  )

  requires = "pa_ppx_hashcons.runtime"
|} version)
