(**/**)

module Private : sig
  val increment_of_heap : Gc.control -> int -> int
  (** [increment_of_heap ctrl words] is the minimum size in words that
      [heap_words] will grow by when allocating [words] in total. Calls
      [Gc.quick_stat ()] if needed. This is only relevant for OCaml <5 for now,
      since the increment is always 0 on 5.x. However for future-proofing this
      is retained (in case the increment is reintroduced). Note that if [words]
      is allocated in multiple allocation calls (e.g. a list), then the heap may
      grow multiple times. *)
end
