module Private = struct
  module Reservation = struct
    module Raw = struct
      type t

      external map : int -> t = "stub_reservation_map_noalloc" [@@noalloc]

      external is_valid : t -> bool = "stub_reservation_is_valid_noalloc"
      [@@noalloc]

      external unmap : t -> int -> bool = "stub_reservation_unmap_noalloc"
      [@@noalloc]
    end

    type t = { raw : Raw.t; bytes : int }

    let[@inline] size_in_bytes t = t.bytes
    let empty = { raw = Raw.map 0; bytes = 0 }

    let[@inline] map bytes =
      let raw = Raw.map bytes in
      if Raw.is_valid raw then { raw; bytes } else empty

    let[@inline] is_valid t = Raw.is_valid t.raw
    let[@inline] unmap t = Raw.unmap t.raw t.bytes
  end

  let[@inline] try_alloc bytes =
    let open Reservation in
    let t = Raw.map bytes in
    Raw.is_valid t && Raw.unmap t bytes

  let reservation = Atomic.make Reservation.empty

  let[@inline] set_and_unmap_old t =
    Reservation.unmap (Atomic.exchange reservation t)

  let () =
    at_exit (fun () ->
        let (_ : bool) = set_and_unmap_old Reservation.empty in
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

  external malloc_trim : int -> bool = "stub_malloc_trim" [@@noalloc]

  let reserve_minor_heaps bytes =
    (* Allocating small OCaml values may use an [mmap] backed sizeclass pool allocator,
       and may not be able to reuse memory that was released by [free(3)]
       (e.g. when garbage collecting large OCaml values)
       If libc supports it, then try to release the memory held after [free(3)].
     *)
    let (_ : bool) = malloc_trim 0 in
    (* free old first, otherwise we might need double the memory when we're already low *)
    set_and_unmap_old Reservation.empty
    && set_and_unmap_old (Reservation.map bytes)

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

  let safe_compact_fails = Atomic.make 0

  let[@inline] atomic_incr a =
    let (_ : int) = Atomic.fetch_and_add a 1 in
    ()

  let[@inline] safe_gc_op op =
    let (_ : bool) = set_and_unmap_old Reservation.empty in
    (* [Gc.full_major ()] and [Gc.compact()] has an implicit minor collection,
       which could crash with [caml_fatal_error] if we are out of memory.
       This can happen even if there'd be enough memory to free in the collection cycle.

       Free the reservation first to ensure the minor collection doesn't crash.
     *)
    op ();
    reserve_minor_heaps ()

  let[@inline never] safe_major_and_compact () =
    (* attempt to perform a full Gc cycle first, this should be faster than a full compaction *)
    if not @@ safe_gc_op Gc.full_major then
      (* a GC cycle was not enough, try to compact *)
      if not @@ safe_gc_op Gc.compact then
        (* we are now running without a reservation, and may crash.
           Although the next call to [check_low_memory] will try again.
         *)
        atomic_incr safe_compact_fails

  let check_low_memory () =
    let bytes = Atomic.get reservation |> Reservation.size_in_bytes in
    let is_low = not @@ try_alloc bytes in
    if is_low then safe_major_and_compact ();
    is_low

  let custom_counter = Atomic.make 0
  let next_allocated_check = Atomic.make 0.

  let[@inline] allocated custom_allocated_words =
    let custom =
      Atomic.fetch_and_add custom_counter custom_allocated_words
      + custom_allocated_words
      |> float_of_int
    in
    let _, promoted, major = Gc.counters () in
    custom +. promoted +. major

  let last_try_alloc = Atomic.make 0

  let rec find_try_alloc bytes =
    if bytes < 1 lsl 18 then 0
    else if try_alloc bytes then bytes
    else find_try_alloc (bytes lsr 1)

  let check_low_memory_custom custom_allocated_words =
    if allocated custom_allocated_words > Atomic.get next_allocated_check then begin
      let is_low = check_low_memory () in
      if is_low then begin
        (* only use optimized path when we're not low on memory *)
        let bytes = Atomic.get reservation |> Reservation.size_in_bytes in
        let bytes = Int.max bytes @@ Atomic.get last_try_alloc in
        let bytes = bytes lsl 1 in
        let bytes = Int.min (1 lsl 30) bytes in
        let bytes = find_try_alloc bytes in
        Atomic.set last_try_alloc bytes;
        (* we have room for [bytes], set next trigger to half *)
        Atomic.set next_allocated_check
          (allocated 0 +. float_of_int (bytes lsr 1))
      end;
      is_low
    end
    else false
end
