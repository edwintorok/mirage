#define CAML_NAME_SPACE
#include "caml/misc.h"
#include "caml/mlvalues.h"

#include <sys/mman.h>

CAMLprim value stub_probe_map_noalloc_untagged(intnat bytes) {
  if (bytes <= 0)
    return Val_false;
  mlsize_t len = bytes;
  void *ptr = mmap(NULL, len, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  return Val_bool((MAP_FAILED != ptr) && !munmap(ptr, len));
}
