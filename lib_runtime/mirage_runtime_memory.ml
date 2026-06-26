(* TODO: distinguish between emergency low memory,
   and low memory in general (TODO: better names).
   In one case we compact, drop caches, etc.
   In the other case we simply limit TCP buffer sizes,
   and rate limit new connections, etc. *)
module Private = struct
  external try_alloc_bytes : int -> bool = "stub_can_alloc"
  (** [try_alloc_bytes bytes] allocates and immediately frees [bytes]. Does not raise
      exceptions on out of memory.

      If this succeeds, then it is likely that future OCaml or custom value
      ({!module:Bigarray}) allocations less than [bytes] would also succeed.

      A failure is not a guarantee that future allocations would fail (e.g.
      [malloc] may have some cached mappings it can use instead of calling
      [mmap]).

      @return [true] if the allocation test succeeded, [false] otherwise
      @raise [Unix_error]
        if memory allocation failed for a reason other than [ENOMEM], or if
        memory deallocation failed *)

  (*let[@inline] percentage words percentage = words / 100 * percentage

  let[@inline] total ctrl live_words =
    live_words + percentage live_words ctrl.Gc.space_overhead

  let[@inline] bytes_of_words w = w * Sys.word_size / 8

  let needed_free_bytes ~heap_words ~custom_bytes =
    let ctrl = Gc.get () and qstat = Gc.quick_stat () in
    let needed_heap_size =
      total ctrl (heap_words + qstat.live_words + ctrl.Gc.minor_heap_size)
    and needed_custom_words =
      percentage qstat.heap_words ctrl.custom_major_ratio
      + percentage ctrl.Gc.minor_heap_size ctrl.Gc.custom_minor_ratio
    in
    let needed_heap_growth = Int.max 0 (needed_heap_size - qstat.heap_words) in
    total ctrl custom_bytes
    (* gc-pacing-new will apply space-overhead to custom words too *)
    + bytes_of_words (needed_heap_growth + needed_custom_words) *)

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
  (*  Private.malloc_trim 0n |> ignore;*)
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
