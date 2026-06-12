#define CAML_NAME_SPACE
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

#include <assert.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <string.h>

/* [caml_ba_finalize] is hidden by CAML_INTERNALS,
   so store at runtime the custom_operations used by the bigarray.
   This should also support mmaped files */

struct custom_operations_proxy {
  struct custom_operations current;
  struct custom_operations proxy;
  atomic_uintnat *counter;
};

static_assert(0 == offsetof(struct custom_operations_proxy, current),
              "struct offset");

static inline bool proxy_is_last(const struct caml_ba_proxy *proxy) {
  /* called before the underlying bigarray finalizer is run,
     so we have to check for 1, not 0 here */
  return !proxy ||
         1 == atomic_load_explicit(&proxy->refcount, memory_order_acquire);
}

static void custom_proxy_finalize(value v) {
  CAMLassert(Custom_tag == Tag_val(v));
  struct custom_operations_proxy *p =
      (struct custom_operations_proxy *)Custom_ops_val(v);
  CAMLassert(p && p->proxy.finalize);

  /* check whether the refcount would reach zero,
     before calling the finalizer.
     The finalizer might free the proxy, so we cannot access it after the call
   */
  if (proxy_is_last(Caml_ba_array_val(v)->proxy)) {
    (void)atomic_fetch_sub(p->counter, caml_ba_byte_size(Caml_ba_array_val(v)));
  }

  /* call the original finalizer */
  p->proxy.finalize(v);

  caml_stat_free(p);
}

static struct custom_operations_proxy *
custom_proxy_init(const struct custom_operations *orig, atomic_uintnat *counter,
                  size_t size) {
  CAMLassert(orig);
  CAMLassert(counter);
  struct custom_operations_proxy *proxy = caml_stat_alloc(sizeof(*proxy));
  /* copy all operations from the underlying bigarray,
     except the finalizer */
  proxy->proxy = *orig;
  proxy->current = proxy->proxy;
  proxy->current.finalize = custom_proxy_finalize;
  proxy->counter = counter;
  (void)atomic_fetch_add(proxy->counter, size);
  return proxy;
}

static atomic_uintnat bigarray_bytes;

CAMLprim value stub_bigarray_track(value ba) {
  CAMLparam1(ba);
  CAMLlocal1(res);

  CAMLassert(Custom_tag == Tag_val(ba));

  if (CAML_BA_EXTERNAL == (Caml_ba_array_val(ba)->flags & CAML_BA_MANAGED_MASK))
    caml_invalid_argument("expected managed bigarray");

  if (!Caml_ba_array_val(ba)->proxy)
      caml_invalid_argument("expected subarray");

  size_t asize =
      SIZEOF_BA_ARRAY + Caml_ba_array_val(ba)->num_dims * sizeof(intnat);

  res = caml_alloc_custom(Custom_ops_val(ba), asize, 0, 0);
  /* cannot copy by direct assignment due to the flexible array member,
     use memcpy */
  memcpy(Caml_ba_array_val(res), Caml_ba_array_val(ba), asize);
  (void)atomic_fetch_add(&Caml_ba_array_val(res)->proxy->refcount, 1);

  struct custom_operations_proxy *proxy =
      custom_proxy_init(Custom_ops_val(ba), &bigarray_bytes,
                        caml_ba_byte_size(Caml_ba_array_val(ba)));
  Custom_ops_val(res) = &proxy->current;

  CAMLreturn(res);
}

CAMLprim value stub_bigarray_track_get_bytes(value u) {
  return Val_long(atomic_load_explicit(&bigarray_bytes, memory_order_acquire));
}
