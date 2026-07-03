module Private = struct
  module Reservation = struct
    type t

    external map : int -> t = "stub_reservation_map_noalloc" [@@noalloc]

    external size_in_bytes : t -> int = "stub_reservation_size_in_bytes_noalloc"
    [@@noalloc]

    external unmap : t -> bool = "stub_reservation_unmap_noalloc" [@@noalloc]

    let empty = map 0
  end

  let try_alloc bytes =
    let open Reservation in
    let t = map bytes in
    let res = size_in_bytes t > 0 in
    res && unmap t

  let reservation = Atomic.make Reservation.empty

  let exchange_and_unmap_old t =
    Reservation.unmap (Atomic.exchange reservation t)

  let () =
    at_exit (fun () ->
        let (_ : bool) = exchange_and_unmap_old Reservation.empty in
        ())

  let domain_count = 1 (* TODO *)

  let[@inline] minor_heaps_size_words () =
    domain_count * (Gc.get ()).minor_heap_size

  let[@inline] round_up n ~multiple_of =
    (n + multiple_of - 1) / multiple_of * multiple_of

  let[@inline] round_up_page_size w = round_up w ~multiple_of:512

  let rec increment_percentage ~percentage ~requested_heap_words ~heap_words =
    if heap_words < requested_heap_words then
      let heap_words =
        round_up_page_size @@ (heap_words + (heap_words / 100 * percentage))
      in
      (increment_percentage [@tailcall]) ~percentage ~requested_heap_words
        ~heap_words
    else heap_words

  let[@inline] increment_of_heap ctrl requested_words =
    match ctrl.Gc.major_heap_increment with
    | 0 ->
        (* only possible value on OCaml 5.x,
            as an optimization we don't need to call [Gc.quick_stat].
            when we grow the heap we always grow it by at least one page.
          *)
        round_up_page_size requested_words
    | percentage when percentage <= 1000 ->
        (* increments <= 1000 are percentages *)
        let heap_words = (Gc.quick_stat ()).heap_words in
        let requested_heap_words = heap_words + requested_words in
        let heap_words' =
          increment_percentage ~percentage ~requested_heap_words ~heap_words
        in
        heap_words' - heap_words
    | words ->
        (* increments > 1000 are fixed number of words *)
        assert (words > 0);
        round_up requested_words ~multiple_of:(round_up_page_size words)

  let reservation_size_bytes () =
    let ctrl = Gc.get () in
    (* finalizers need 3 words for [struct final] for every 2 words in the minor heap,
       and there might also be some overhead when allocating small values
       (~11% on OCaml 5.x (see sizeclasses.h), and ~18%(measured) on OCaml 4.x, so round up to 50%) 
       1 + 3/2 + 1/2 = 3
      *)
    let words = minor_heaps_size_words () in
    let words_with_overhead = words * 3 / 2
    and finalizer_overhead = words * 3 / 2
    and custom_minor_words =
      (* try to prevent [Out_of_memory] from [Bigarray] allocations *)
      words / 100 * ctrl.custom_minor_ratio
    in
    let words =
      increment_of_heap ctrl words_with_overhead
      + finalizer_overhead
      + custom_minor_words
    in
    words * Sys.word_size / 8

  let reserve_minor_heaps bytes =
    let t = Reservation.map bytes in
    let ok = Reservation.size_in_bytes t > 0 in
    (* only replace the reservation on a successful allocation *)
    ok && exchange_and_unmap_old t

  let reserve_minor_heaps () =
    (* Do not call [Gc.quick_stat ()] here, it may trigger a minor Gc and crash 
       if we're out of memory.
       Also when called from [safe_compact] this is expected to be 0 and fail.
       And up-to-date size will get computed after this succeeds.
     *)
    let bytes = Atomic.get reservation |> Reservation.size_in_bytes in
    reserve_minor_heaps bytes
    &&
    (* passed the reservation test, safe to allocate OCaml values *)
    let bytes = reservation_size_bytes () in
    Atomic.get reservation |> Reservation.size_in_bytes == bytes
    || reserve_minor_heaps bytes

  external malloc_trim : int -> bool = "stub_malloc_trim" [@@noalloc]

  let safe_compact_fails = Atomic.make 0

  let[@inline] atomic_incr a =
    let (_ : int) = Atomic.fetch_and_add a 1 in
    ()

  let[@inline never] safe_major_and_compact () =
    (* Compaction has an implicit minor collection, which could crash
       with [caml_fatal_error] if we are out of memory.
       This can happen even if there'd be enough memory to free in the collection cycle.

       Free the reservation first to ensure the minor collection doesn't crash.
     *)
    let (_ : bool) = exchange_and_unmap_old Reservation.empty in

    (* attempt to perform a full Gc cycle first, this should be faster than a full compaction *)
    Gc.full_major ();
    (* Allocating small OCaml values may use an [mmap] backed sizeclass pool allocator,
       and may not be able to reuse memory that was released by [free(3)]
       (e.g. when garbage collecting large OCaml values)
       If libc supports it, then try to release the memory held after [free(3)].
     *)
    let (_ : bool) = malloc_trim 0 in
    if not @@ reserve_minor_heaps () then begin
      (* a GC cycle was not enough, try to compact *)
      Gc.compact ();
      (* compaction may have released some memory,
         but not fully, repeat [malloc_trim(3)].
       *)
      let (_ : bool) = malloc_trim 0 in
      if not @@ reserve_minor_heaps () then
        (* we are now running without a reservation, and may crash.
           Although the next call to [check_low_memory] will try again.
         *)
        atomic_incr safe_compact_fails
    end

  let check_low_memory () =
    let is_low = not @@ reserve_minor_heaps () in
    if is_low then safe_major_and_compact ()
end
