#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "compat.h"

#include <stddef.h>
#include <sys/mman.h>

CAMLprim value stub_reservation_is_valid_noalloc(value const val_ptr) {
  return Val_bool(!!Ptr_val(val_ptr));
}

#define Val_null (Val_ptr(NULL))

CAMLprim value stub_reservation_map_noalloc(value const val_bytes) {
  /* noalloc: avoid CAMLparam */
  long const bytes = Long_val(val_bytes);
  if (bytes <= 0)
    return Val_null;

  /* noalloc only from an OCaml point of view, we do call mmap(2) */

  void *const ptr =
      mmap(NULL, bytes, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  /* storing the length would require remapping the beginning as
     PROT_READ|PROT_WRITE, which may fail */

  /* Can't raise exceptions.
     MAP_FAILED is not aligned, so can't use it directly in [Val_ptr].
     Use [Val_null] instead */
  return MAP_FAILED == ptr ? Val_null : Val_ptr(ptr);
}

CAMLprim value stub_reservation_unmap_noalloc(value const val_ptr,
                                              value const val_bytes) {
  void *const ptr = Ptr_val(val_ptr);
  long const bytes = Long_val(val_bytes);
  /* if this is a failed mapping, then unmapping always succeeds as a no-op */
  return Val_bool(!ptr || (bytes > 0 && !munmap(ptr, bytes)));
}

#ifdef __GLIBC__
#include <malloc.h>
#else

static int malloc_trim(size_t pad) { return 0; }
#endif

CAMLprim value stub_malloc_trim_noalloc(value const val_pad) {
  long const pad = Long_val(val_pad);
  return Val_bool(pad >= 0 && malloc_trim(pad));
}
