#define CAML_NAME_SPACE
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

static value stub_alloc_initialized_array(mlsize_t count) {
  CAMLparam0();
  CAMLlocal1(result);
  /* An allocation failure during minor heap promotion would immediately
     crash with [caml_fatal_error].
     Therefore allocate into the major heap directly, where [Out_of_memory]
     can be raised.
   */
  result = caml_alloc_shr(count, 0);
  /* Ensure the array is fully initialized, before allocating more. */
  for (size_t i = 0; i < count; i++) {
    caml_initialize(&Field(result, i), Val_unit);
  }
  CAMLreturn(result);
}

CAMLprim value stub_alloc_shr(value val_total) {
  CAMLparam1(val_total);
  CAMLlocal1(result);

  long count = Long_val(val_total) / Max_young_wosize;
  if (count <= 0)
    caml_invalid_argument("stub_alloc_shr: total > Max_young_wosize");

  count += Max_young_wosize;
  result = stub_alloc_initialized_array(count);
  size_t i;
  /* use each sizeclass at least once */
  for (i = 0; i < Max_young_wosize / 2; i += 2) {
    if (i > 0)
      Store_field(result, i, stub_alloc_initialized_array(i));
    Store_field(result, i + 1,
                stub_alloc_initialized_array(Max_young_wosize - i));
  }

  for (; i < count; i++) {
    Store_field(result, i, stub_alloc_initialized_array(Max_young_wosize));
  }

  CAMLreturn(result);
}
