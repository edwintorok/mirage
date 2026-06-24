#define CAML_NAME_SPACE
#include "caml/fail.h"
#include "caml/memory.h"

#ifdef __GLIBC__
#include <malloc.h>
#else
static int malloc_trim(size_t pad) { return 0; }
#endif

static inline mlsize_t Size_val(value val_long) {
  long l = Long_val(val_long);
  if (l <= 0)
    caml_invalid_argument("Size_val");
  return l;
}

/* stub_make_array_shr(count) allocates an initialized array of [count] words. */
static inline value stub_make_array_shr(mlsize_t count) {
  value result = caml_alloc_shr(count, 0);
  for (size_t i = 0; i < count; i++)
    /* this is what caml_alloc() would do */
    Field(result, i) = Val_unit;
  return result;
}

CAMLprim value stub_alloc_array_shr(value val_count, value val_size) {
  CAMLparam2(val_count, val_size);
  CAMLlocal1(result);

  mlsize_t count = Size_val(val_count);
  mlsize_t size = Size_val(val_size);

  result = stub_make_array_shr(count);

  for (size_t i = 0; i < count; i++) {
    Store_field(result, i, stub_make_array_shr(size));
  }

  CAMLreturn(result);
}

CAMLprim value stub_malloc_trim_noalloc(intnat amount) {
  /* The OCaml runtime uses both malloc() and mmap().
     If malloc keep pages allocated, then it might be unavailable to mmap.
     When we are low on memory tell the allocator to give back the pages
     (according to malloc_trim(3) since Glibc 2.8 this'll also free whole pages
      anywhere, not just at the end).
     This has no effect when custom allocators (jemalloc, mimalloc) are linked,
     because the this call is not intercepted and the call will be made to
     glibc's allocator.
   */
  return amount >= 0 ? Val_bool(malloc_trim(amount)) : Val_false;
}
