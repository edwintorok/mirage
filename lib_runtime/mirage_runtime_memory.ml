module Private = struct
  external malloc_trim : nativeint -> bool = "stub_malloc_trim_noalloc" [@@noalloc]
  external alloc_array_shr: int -> int -> unit array array = "stub_alloc_array_shr"

  module Reservation = struct
    (** free and reusable words in the OCaml heap for promoting values from the
        minor heap *)
    let reserved_words = Atomic.make 0

    let finalise reservation =
      let (_ : int) =
        Atomic.fetch_and_add reserved_words (Array.length reservation + 1)
      in
      (* these words are free in the OCaml heap, but not yet released to the OS until the next compaction.
         it can be reused by other minor heap promotions.
       *)
      ()

    let alloc_words words =
      assert (words > 1);
      let r = Array.make (words - 1) 0 in
      Gc.finalise finalise r;
      r

    let max_young_wosize = 256

    let (_ : Gc.alarm) =
      let compactions = ref 0 in
      Gc.create_alarm (fun () ->
          let latest_compactions =
            let open Gc in
            (quick_stat ()).compactions
          in
          if latest_compactions > !compactions then begin
            (* after a compaction assume that everything that was free got released,
             so we don't have any reserved words anymore
           *)
            Atomic.set reserved_words 0;
            compactions := latest_compactions
          end)

    let reserve () =
      let minor_heap_size = Gc.(get ()).minor_heap_size in
      let per_iteration_size = minor_heap_size / (max_young_wosize - 1) in
      let offset =
        (* can't allocate 0 or 1 words *)
        2
      in
      let a =
        Array.init (max_young_wosize - offset) @@ fun i ->
        let wosize = i + offset in
        per_iteration_size / wosize |> alloc_words
      in
      (* ensure they get promoted to the major heap *)
      Gc.minor ();
      let _alive = Sys.opaque_identity a in
      ()

    let in_reserve = Atomic.make 0

    let in_reserve_finally () =
      let (_ : int) = Atomic.fetch_and_add in_reserve (-1) in
      ()

    let reserve () =
      (* avoid nested calls if this function is invoked from a Gc callback *)
      let currently_in_reserve = Atomic.fetch_and_add in_reserve 1 in
      if currently_in_reserve = 0 then
        Fun.protect ~finally:in_reserve_finally reserve
  end

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
        0
    | percentage when percentage <= 1000 ->
        (* increments <= 1000 are percentages *)
        let heap_words = Gc.(quick_stat ()).heap_words in
        let requested_heap_words = heap_words + requested_words in
        let heap_words' =
          increment_percentage ~percentage ~requested_heap_words ~heap_words
        in
        heap_words' - heap_words
    | words ->
        (* increments > 1000 are fixed number of words *)
        assert (words > 0);
        round_up requested_words ~multiple_of:(round_up_page_size words)

  let[@inline] major_heap_increment_words ctrl =
    let words = ctrl.Gc.minor_heap_size in
    let overhead =
      (* on OCaml 5.x there is at most 11% overhead, see gen_sizeclasses.ml *)
      11
    in
    let overhead' =
      (* measured with the unit test on 4.14.3 *)
      18
    in
    let request = words + (words / 100 * overhead)
    and request' = words + (words / 100 * overhead') in
    Int.max request (increment_of_heap ctrl request') |> round_up_page_size

  let bytes_of_words w = w * Sys.word_size / 8

  let[@inline] minor_heap_needed_bytes () =
    let ctrl = Gc.get () in
    major_heap_increment_words ctrl |> bytes_of_words

  let[@inline] has_room_for_bytes bytes =
    (* TODO: if you are using a multiple domains, also multiply *)
    bytes + (2 * minor_heap_needed_bytes ()) |> try_alloc_bytes
end

let on_low_memory = ref []
let register_on_low_memory f = on_low_memory := f :: !on_low_memory
let call_low_memory f = f ()

let low_memory_cleanup () =
  Private.malloc_trim 0n |> ignore;
  Gc.full_major ();

  (* if custom memory (e.g. bigarrays) got allocated then
       we may run out of room, even if a previous call to [check_..] was OK.
       Try compacting as a last resort, if we don't have enough room for the minor heap either.
       Also according to the manual 2 calls might be needed anyway if ephemerons
       are used (this is the 2nd).
    *)
  if not @@ Private.has_room_for_bytes 0 then Gc.compact ();

  (* call the low memory callbacks, it is safer to do this
       after we attempted to free some memory already *)
  List.iter call_low_memory !on_low_memory

let check_room_for_bytes bytes =
  if Private.has_room_for_bytes bytes then true
  else begin
    low_memory_cleanup ();
    false
  end
