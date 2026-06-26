#define CAML_NAME_SPACE
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/threads.h"
#include "caml/unixsupport.h"

#include <errno.h>
#include <stdbool.h>
#include <sys/mman.h>

CAMLprim value stub_probe_mmap(value val_bytes) {
  mlsize_t len = Long_val(val_bytes);
  int saved_errno = 0;
  const char *cmdname = NULL;
  bool ok = false;

  caml_release_runtime_system();
  void *ptr = mmap(NULL, len, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  if (MAP_FAILED == ptr) {
    if (ENOMEM != errno) {
      cmdname = "mmap";
      saved_errno = errno;
    }
  } else if (munmap(ptr, len) < 0) {
    cmdname = "munmap";
    saved_errno = errno;
  } else {
    ok = true;
  }
  caml_acquire_runtime_system();

  if (cmdname)
    caml_unix_error(saved_errno, cmdname, Nothing);
  return Val_bool(ok);
}
