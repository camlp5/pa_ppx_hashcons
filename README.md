A PPX Rewriter for Hashconsing

### Version

This is ``pa_ppx_hashcons`` (alpha) version 0.10.

# Overview

This PPX rewriter is a mechanization of the design and code by
Jean-Christophe Filliatre and Sylvain Conchon, from their paper
[Type-Safe Modular Hash-Consing](https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf).
The only deviation from the paper, is in the treatment of memoization,
which is discussed below.  This work was directly inspired by
discussions with the first paper author.

In the following discussion, I assume that the reader knows what
hashconsing is, and the tradeoffs that obtain when using this
technique.  There are many sources for this information, and for
example I refer to the above paper for an excellent discussion.

# Usage

This rewriter is a deriver (in the style of [PPX Deriving](https://github.com/ocaml-ppx/ppx_deriving),
but implemented using [Camlp5](https://github.com/camlp5/camlp5)
and [pa_ppx](https://github.com/chetmurthy/pa_ppx).  To use it, add
attributes to type-declarations, e.g. (from `tests/test_hashcons.ml`)
```
type term =
    Ref of int
  | Abs of term
  | App of term * term[@@hashcons_module Term][@@hashcons_constructor term]
[@@deriving hashcons { module_name = LAM
                     ; memo = {
                         memo = [%typ: term]
                       ; memo_int = [%typ: int]
                       ; memo2_int_term = [%typ: int * term]
                       ; memo2_term_term = [%typ: term * term]
                       }
                     }]
```
and compile with this package
```
ocamlfind ocamlc -c -package hashcons,pa_ppx_hashcons -syntax camlp5o  -i test_hashcons.ml
```

Almost all the examples are all with a single type, but there is an
example of the entire OCaml AST from Camlp5: which comprises a large
number of mutually-recursive types.

# Usage Details

A description of the format of that `[@@deriving ...]` attribute:

1. `hashcons_module_name` is not optional, and is used to specify a module
   which will be created and in which all new types and value
   definitions will be placed.  This is necessary since the new types
   are very similar to the old ones, and we need to put them in a
   different structure.

2. `normal_module_name` is not optional, and specifies a module which
   will be created and filled with the normal version of the type
   (using type-equations so it's equal to the original type).  This is
   useful in the case where the types being hashconsed are imported via
   `pa_ppx_import`.
   
3. `hashcons_module` is an item-attribute of each type, and specifies
   the module-name that will be generated for the hashtable of values
   of that type.  This is optional and can be omitted (and will be
   given a default value of `HC_<type-name>`).
   
4. `hashcons_constructor` is an item-attribute of each type, and
   specifies the name to be used for the smart-constructor of each
   type, that does hash-consing.  It is optional, with default value
   the same as the type-name.

5. `memo` is an optional option with an argument that is a
   record-value, with each field of the form
   ` <memo_function_name> = [%typ: <type-or-conjunction-of-types>] `.
   A memo-function of the specified name will be generated, e.g.
   for `memo2_int_term = [%typ: int * term]` a function
   `memo2_int_term : (int -> term -> 'a) -> (int -> term -> 'a)`
   is generated that memoizes the function.

   The types in the conjunction must be either primitive types, or
   hashconsed types, or conjunctions of hashconsed types, or
   recursively conjunctions of the latter.  So for instance,
   `((term * term) * term) * term` is allowed.

6. For complex memoizers, the PPX rewriter may produce error-messages like:
```
File "test_hashcons.ml", line 4, characters 1-1128:
Failure: find_matching_memo: no match:
Please declare a memoizer of type <<int>>
```

This means that (as explained below) a memoizer you declared, needs to
recursively memoize a function of the specified type.  Because this
isn't so common, it's simpler for the user to specify that subsidiary
memoizer. If this becomes a real burden, I can change things so that
the code automatically constructs it.  But this way, the subsidiary
memoizer gets a name the user has selected.

# How Does the Memoization Work?

As the paper discusses, the hashtables used to hold the values of the
hashconsed type are weak hashtables, so when those values are no
longer reachable outside the hashtable, their pointers in the
hashtable are cut and the values are garbage-collected.  However, in
the paper the memoization solution presented, which is to use the
unique-index value (`tag`) as a key for the memoization hashtable, can
lead to a memory leak.  The first author let us know about this, and
about a solution using ephemerons, which we have implemented in this
package.  Here we'll sketch our solution, explaining it in stages.  We
won't include any code, because the reader can look at the generated
code in the file `tests/test_hashcons.cmo.ppo.ml` and see examples
galore.

1. Suppose we want to memoize a function `f : t_1 -> .... t_n -> rty`.
   Without loss of generality, we can reorder the arguments into first
   the types that are hash-consed, and then the primitive types., viz.
   `hct_1 -> ... -> hct_n -> pt_1 -> ... -> pt_m -> rty`.

2. To memoize a function of one hashconsed argument, e.g. `term ->
   rty` we can apply a single-argument Ephemeron hashtable.  You can
   see an example of the code for this in the memoizer `memo_term` in
   the above-mentioned file.
   
3. Similarly, a function of two hashconsed arguments (e.g. `term ->
   term -> rty`) can be memoized by applying a two-argument Ephenerom
   hashtable.  The memoizer `memo_term_term` shows this.
   
4. a function of only non-hash-consed arguments `f : pt_1 -> ... -> pt_m -> rty`
   is memoized with a memoizer of type:
   ```
   (pt_1 * ... * pt_n, rty) Hashtbl.t -> (pt_1 -> ... -> pt_m -> rty) -> (pt_1 -> ... -> pt_m -> rty)`.
   ```
   
   So the memoizer is first applied to a hashtable, and then the function, and then its arguments.
   The memoizer `memo_int_int_int_int` shows this.

5. A function `f : hct_1 -> ... -> hct_n -> pt_1 -> ... -> pt_m -> rty` can be treated in the
   following manner.  By example we'll treat `term -> term -> term -> term -> int -> int -> rty`.

   a. memoize the function `fun x y -> (x,y) : term -> term -> term * term`

   b. memoize the function `fun x y -> (x,y) : (term * term) -> term -> (term * term) * term`

   c. memoize the function `fun x y -> (x,y) : ((term * term) * term) -> term -> ((term * term) * term) * term`

   Note that these steps are just to convert 4 arguments into a single
   argument, in such a way that all the pieces are stored in Ephenerom
   hashtables.  There is an N-ary ephenerom hashtable (and we could
   have used it, since all the types are identical) but we wanted to
   able to handle the case where all the arguments are of different
   types.

   d. memoize the function `fun _ -> Hashtbl.create 251 : ((term * term) * term) * term -> (int * int, rty) Hashtbl.t`

   It should be clear how to put all these pieces together:

   - given `w,x,y,z : term, i, j : int`,
   - apply step (a) to `w, x` yielding `p`
   - then step (b) to `p, y` yielding `q`
   - then step (c) to `q, z` yielding `r`
   - then step (d) to `r`, yielding the hashtable `s`
   - finally, we can apply step #4 above to the hashtable `s` and `(i,j)` and the original
   function invocation `f w x y z`, to complete the memoization.  Again, the generated code
   in the previously mentioned testcase shows this sequence in straightforward detail.
