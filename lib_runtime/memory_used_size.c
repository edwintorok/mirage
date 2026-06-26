#define CAML_NAME_SPACE
#include "caml/misc.h"
#include "caml/mlvalues.h"


CAMLprim value stub_memory_used(value u) {
    return Caml_state->extra_heap_resources * Caml_state->shared_heap->
}
