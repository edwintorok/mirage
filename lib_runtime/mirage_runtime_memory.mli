val register_on_low_memory : (unit -> unit) -> unit
(** [register_on_low_memory f] registers [f] to be called when we run low on
    memory, but before an {!Out_of_memory} exception is raised. [f] should try
    to minimize its own memory allocations, and make values unreachable (e.g.
    caches, TCP buffers). It should not call the garbage collector itself. The
    registered callbacks will be called from {!val:check_room_for_bytes}. The
    functions should not raise any exceptions, if they do, then
    [check_room_for_bytes] will reraise the exception. *)

val low_memory_cleanup : unit -> unit
(** [low_memory_cleanup ()] calls [malloc_trim] if available, runs the garbage
    collector, and the registered on_low_memory handlers. *)

val check_room_for_bytes : int -> bool
(** [check_room_for_bytes bytes] checks whether there is enough room for [bytes]
    plus minor heap promotion. If there isn't enough room then {!Gc.full_major},
    {!Gc.compact} and the registered low memory callbacks may be called as
    needed.

    This can be used to decide whether a network packet should be dropped, or
    whether a new TCP connection should be accepted. *)

(**/**)

module Private : sig
  val try_alloc_bytes : int -> bool
  (** [try_alloc_bytes bytes] allocates and immediately frees [bytes]. Does not
      raise exceptions on out of memory.

      If this succeeds, then it is likely that future OCaml or custom value
      ({!module:Bigarray}) allocations less than [bytes] would also succeed.

      A failure is not a guarantee that future allocations would fail (e.g.
      [malloc] may have some cached mappings it can use instead of calling
      [mmap]).

      @return [true] if the allocation test succeeded, [false] otherwise
      @raise [Unix_error]
        if memory allocation failed for a reason other than [ENOMEM], or if
        memory deallocation failed *)

  val increment_of_heap : Gc.control -> int -> int
  (** [increment_of_heap ctrl words] is the minimum size in words that
      [heap_words] will grow by when allocating [words] in total. Calls
      [Gc.quick_stat ()] if needed. This is only relevant for OCaml <5 for now,
      since the increment is always 0 on 5.x. However for future-proofing this
      is retained (in case the increment is reintroduced). Note that if [words]
      is allocated in multiple allocation calls (e.g. a list), then the heap may
      grow multiple times. *)

  val major_heap_increment_words : Gc.control -> int
  (** [major_heap_increment_words ctrl] is the amount of words a full major heap
      will grow when running the minor GC on a full minor heap. *)

  val has_room_for_bytes : int -> bool
  (** [has_room_for_bytes bytes] checks whether there is enough [C] memory left
      if the major heap is full and we need to allocate [minor_heap_size] words
      \+ [bytes]. Note that [Gc.free_words] reports the sum of available memory,
      but due to fragmentation this doesn't mean we have any memory available
      for promoting from the minor heap. The worst case assumption is that we
      need to grow the major heap, and we need to leave enough free memory for
      that to avoid a [caml_fatal_error] crash during promotion.

      For safety we leave enough room for at least 2 minor heaps, because
      running [Gc.compact ()] would trigger a minor collection first, and this
      must not run out of memory. *)
end
