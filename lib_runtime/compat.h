#include "caml/version.h"

#if OCAML_VERSION < 50000
#include "caml/mlvalues.h"
/* backport from mlvalues.h in 5.x:
   encode C pointers as OCaml integers to avoid 'naked pointers' */
Caml_inline value Val_ptr(void *p) {
  CAMLassert(((value)p & 1) == 0);
  return (value)p + 1;
}

Caml_inline void *Ptr_val(value val) {
  CAMLassert(val & 1);
  return (void *)(val - 1);
}
#endif
