#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/version.h"

#include <stddef.h>
#include <sys/mman.h>

#if OCAML_VERSION < 50000
Caml_inline value Val_ptr(void *p) {
  CAMLassert(((value)p & 1) == 0);
  return (value)p + 1;
}

Caml_inline void *Ptr_val(value val) {
  CAMLassert(val & 1);
  return (void *)(val - 1);
}
#endif

struct reservation {
  long len;
};

Caml_inline value Val_reservation(struct reservation *r) { return Val_ptr(r); }

Caml_inline struct reservation *Reservation_val(value val) {
  return Ptr_val(val);
}

Caml_inline long len_reservation(const struct reservation *r) {
  return r ? r->len : -1;
}

CAMLprim value stub_reservation_size_in_bytes_noalloc(value val_ptr) {
  return Val_long(len_reservation(Reservation_val(val_ptr)));
}

CAMLprim value stub_reservation_map_noalloc(value val_bytes) {
  long bytes = Long_val(val_bytes);
  struct reservation *res = NULL;

  /* noalloc only from an OCaml point of view, we do call mmap(2) */

  if (bytes >= sizeof(struct reservation)) {
    void *ptr =
        mmap(NULL, bytes, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    /* can't raise exceptions */
    res = MAP_FAILED == ptr ? NULL : ptr;
  }

  if (res) {
    res->len = bytes;
  }

  return Val_reservation(res);
}

CAMLprim value stub_reservation_unmap_noalloc(value val_ptr) {
  struct reservation *r = Reservation_val(val_ptr);
  /* if this is a failed mapping, then unmapping always succeeds as a no-op */
  return Val_bool(!r || !munmap(r, len_reservation(r)));
}
