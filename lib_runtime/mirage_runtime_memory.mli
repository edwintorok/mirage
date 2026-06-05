(**/**)

module Private : sig
  val try_alloc_bytes : int -> bool
  (** [try_alloc_bytes bytes] checks whether [bytes] memory are available using
      the C [malloc(3)] function. This function doesn't raise exceptions, and
      immediately frees any memory allocated. We cannot use
      [Bigarray.Array1.create], because that wouldn't free memory immediately,
      and we'd risk using up all available memory and crashing on the next minor
      heap collection with a fatal error.

      @return [true] if the allocation succeeded, [false] otherwise *)

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
end
