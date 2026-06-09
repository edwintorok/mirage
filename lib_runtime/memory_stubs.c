#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/unixsupport.h"

#include <errno.h>
#include <sys/mman.h>

CAMLprim value stub_try_alloc(value val_bytes) {
  mlsize_t len = Long_val(val_bytes);

  /* do not release the runtime lock here: that'd allow OCaml code to allocate
     more when we might already be low on memory */
  void *ptr = mmap(NULL, len, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  if (MAP_FAILED == ptr) {
    /* do not raise on allocation failure,
       return a boolean instead */
    if (ENOMEM == errno)
      return Val_false;
    uerror("mmap", Nothing);
  }

  if (munmap(ptr, len) < 0)
    uerror("munmap", Nothing);
  return Val_true;
}

#ifdef __GLIBC__
#include <malloc.h>
#else

static int malloc_trim(size_t pad) { return 0; }

#endif

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
