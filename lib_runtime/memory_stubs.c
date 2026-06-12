#define CAML_NAME_SPACE
#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"

#include <stdlib.h>
#if __STDC_VERSION_STRING_H__ >= 202311L
#include <string.h>
#endif

#ifdef __GLIBC__
#include <malloc.h>
#else

static int malloc_trim(size_t pad) { return 0; }

#endif

#include <sys/mman.h>

CAMLprim value stub_malloc_trim(intnat amount) {
  /* The OCaml runtime uses both malloc() and mmap().
     If malloc keep pages allocated, then it might be unavailable to mmap.
     When we are low on memory tell the allocator to give back the pages
     (according to malloc_trim(3) since Glibc 2.8 this'll also free whole pages
      anywhere, not just at the end).
     This has no effect when custom allocators (jemalloc, mimalloc) are linked,
     because the this call is not intercepted and the call will be made to
     glibc's allocator.
   */
  return Val_bool(malloc_trim(Long_val(amount)));
}

/*
 * TODO: maybe pool_allocate should always have some reserved memory,
 * for the minor allocations to not run out
 */
CAMLprim value stub_try_alloc(value val_count, value val_size) {
  CAMLparam2(val_count, val_size);
  CAMLlocal2(result, tmp);
  
  long count = Long_val(val_count);
  if (count <= 0)
      caml_invalid_argument("try_alloc: count must be > 0");

  long size = Long_val(val_size);
  if (size <= 1)
      caml_invalid_argument("try_alloc: size must be > 1");
  
  tmp = caml_alloc(0, 0);

  result = caml_alloc(count, 0);
  /* ensure array is fully initialized */
  for (size_t i = 0;i < count; i++) {
      Store_field(result, i, tmp);
  }

  for (size_t i = 0;i < count; i++) {
      Store_field(result, i, caml_alloc_shr(size, 0));
  }

  CAMLreturn(result);
}

/*
 * TODO: reserve memory with array * 2, then free, full major,
      sum += amount;

      if (sum > 1000000) {
        sum = 0;
        ptr = mmap(NULL, amount, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
        if (MAP_FAILED == ptr)
          return Val_false;
        munmap(ptr, amount);
      }

 * check promoted words, if exceeds reservation / 2, then try reserving again.
 */
