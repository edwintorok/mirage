#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/misc.h"

#include <stdlib.h>
#if __STDC_VERSION_STRING_H__ >= 202311L
#include <string.h>
#endif

#ifdef __GLIBC__
#include <malloc.h>
#endif

/* this will be NULL if no definition available at runtime */
CAMLweakdef int malloc_trim(size_t pad);

CAMLprim value stub_try_alloc(value val_amount)
{
    long amount = Long_val(val_amount);
    if (amount > 0) {
        /* GCC and Clang would optimize [malloc/free] away,
           and make this function always return true,
           because memory allocation isn't considered a side-effect.
           Mark the pointer itself volatile, so that all accesses
           are side-effects. 
           The definition of access is still implementation defined though.
         */
        char* volatile ptr = malloc(amount);
        if (ptr) {
            /* if available we could also call [explicit_bzero] */
#if __STDC_VERSION_STRING_H__ >= 202311L
            /* future proofing against more compiler optimizations:
               ensure we actually store something. */
            (void)memset_explicit(ptr, 1, 1);
#endif
            free(ptr);
            return Val_true;
        }

        /* The OCaml runtime uses both malloc() and mmap().
           If malloc keep pages allocated, then it might be unavailable to mmap.
           When we are low on memory tell the allocator to give back the pages
           (according to malloc_trim(3) since Glibc 2.8 this'll also free whole pages
            anywhere, not just at the end).
           This has no effect when custom allocators (jemalloc, mimalloc) are linked,
           because the this call is not intercepted and the call will be made to glibc's allocator.
         */
        if (malloc_trim)
            malloc_trim(0);
    }
    return Val_false;
}
