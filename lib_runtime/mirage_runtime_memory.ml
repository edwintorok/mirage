external alloc_shr : int -> unit array array = "stub_alloc_shr"
(** [alloc_shr words] allocates [words] in the major heap,
    using the same allocator that minor heap promotions would use.
    Because this runs outside of a minor heap collection, it won't
    crash with [caml_fatal_error ()].

    @raises Out_of_memory if not enough memory is available
 *)

let reserved_ocaml_trigger = Atomic.make 0
let reserved_custom = Atomic.make 0

let reset_reserved () =
  Atomic.set reserved_ocaml_trigger 0;
  Atomic.set reserved_custom 0

let last_compactions = Atomic.make 0

let[@inline] check_compactions () =
  let qstat = Gc.quick_stat () in
  if qstat.compactions > Atomic.get last_compactions then begin
    reset_reserved ();
    Atomic.set last_compactions qstat.compactions
  end

let (_ : Gc.alarm) =
  Gc.create_alarm check_compactions

let atomic_incr a count =
  let (_ : int) = Atomic.fetch_and_add a count in ()

let finalise_array a =
  let wosize = Array.length a + 1 in
  (* until the next compaction this memory is now reusable,
      e.g. for minor heap promotions *)
  atomic_incr reserved_ocaml_trigger wosize

let finalise_custom ba =
  (* [free()] got called, but any cached [mmap] pages are now
     reusable by the next [malloc], until [malloc_trim()] is called.
   *)
  atomic_incr reserved_custom (Bigarray.Array1.size_in_bytes ba)

let track_array a =
  Gc.finalise finalise_array a

let track_custom ba =
  Gc.finalise finalise_custom ba;
  ba

let alloc_minor_custom_values ctrl =
  let bytes = ctrl.Gc.minor_heap_size / 100 * ctrl.Gc.custom_minor_ratio * Sys.word_size / 8 in
  track_custom Bigarray.(Array1.create char c_layout bytes)

let alloc_minor_ocaml_values ctrl =
  let a = alloc_shr ctrl.Gc.minor_heap_size in
  Array.iter track_array a;
  a

let alloc_minor_heap ctrl =
  (* allocate custom first, less likely to call [caml_fatal_error] *)
  let custom = alloc_minor_custom_values ctrl in
  let ocaml = alloc_minor_ocaml_values ctrl in
  custom, ocaml

let reserve_minor_heaps () =
  let ctrl = Gc.get ()
  and domains =
  (* this will usually be 1 for a Solo5/Unikraft unikernel, but keep it generic *)
    Domain.recommended_domain_count () in
  let (_ : _ array) = Array.init (2*domains) (fun _ -> alloc_minor_heap ctrl) |> Sys.opaque_identity in
  (* set trigger to half the allocation size by allocating twice above *)
  atomic_incr reserved_ocaml_trigger (-domains * ctrl.minor_heap_size);
  (* The major collection below should mark the above arrays as garbage,
     but without a call to Gc.compact it shouldn't actually free them.
     (unless automatic compaction is on).
     So the next time the minor Gc runs it should be able to reuse them when promoting OCaml values.
     This should avoid running out of memory during a minor Gc
     (which would lead to a crash by calling [caml_fatal_error ()]).
   *)
  Gc.full_major ()

let low_memory = Atomic.make false

let[@inline never] check_minor_heaps_slowpath () =
  try
    (* replace old reservation with new,
       until the heap is compacted this will eventually become memory
       that a minor collection could use.
     *)
    reserve_minor_heaps ();
    Atomic.set low_memory false;
  with Out_of_memory -> (
    Atomic.set low_memory true;
    (* recover all memory,
       there should still be enough free memory for the implicit
       minor heap collection to work
     *)
    Gc.compact ();
    reset_reserved ();
    (* attempt to reserve again *)
    try reserve_minor_heaps ()
      (* don't change low_memory here, we want at least one Lwt cycle
         where it is set *)
    with Out_of_memory ->
      (* a 2nd compaction might be needed for ephemerons.
         To avoid an infinite loop do not attempt to allocate again here.
         The next call to [check_minor_heaps] will attempt to allocate again.
       *)
      Gc.compact ())

let check_minor_heaps () =
  check_compactions ();
  if Atomic.get reserved_ocaml_trigger <= 0
  then check_minor_heaps_slowpath ()

let (_ : Gc.alarm) =
  Gc.create_alarm check_minor_heaps

let () =
  reserve_minor_heaps ();
  Mirage_runtime.at_enter_iter check_minor_heaps
