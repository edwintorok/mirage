#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/unixsupport.h"

#include <errno.h>
#include <sys/mman.h>

CAMLprim value stub_can_alloc(value val_bytes) {
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
