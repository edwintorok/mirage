(**/**)

module Private : sig
  module Reservation : sig
    type t

    val size_in_bytes : t -> int
    (** [size_in_bytes t] is the amount of bytes reserved.

        @return -1 if the reservation failed *)

    val map : int -> t
    (** [map bytes] reserves [bytes] using [mmap(2)]. Does not raise exceptions,
        and doesn't allocate OCaml values. Use {!val:size_in_bytes} to check
        whether the reservation succeeded.

        This value is not managed by the OCaml GC, so you must call {!val:unmap}
        to release it. *)

    val unmap : t -> bool
    (** [unmap reservation] frees memory associated with [reservation]. This is
        safe to call on failed reservations (it is a no-op and returns true). Do
        not call this multiple times with the same [reservation] (double-free).

        @return [true] if unmapping succeeded, [false] if [munmap(2)] fails. *)
  end

  val try_alloc : int -> bool
  (** [try_alloc bytes] attempts to allocate and immediately free [bytes].
      Doesn't raise exceptions, and doesn't allocate OCaml values.

      @return [true] if the allocation succeeded *)

  val reserve_minor_heaps : unit -> bool
  (** [reserver_minor_heaps ()] reserves enough memory for a minor heap
      collection. This reservation prevents bigarrays and other large OCaml
      values from allocating and fragmenting this region. It'll be released when
      we are low on memory

      @return [true] if it succeeded. *)

  val check_low_memory : unit -> bool
  (** [check_low_memory ()] checks whether enough free memory is available for
      the next minor heap collection to complete without crashing in
      [caml_fatal_error]. If this returns [true] the caller should attempt to
      free some memory. If if returns [false] then an [Out_of_memory] exception
      may still get raised later.

      @return [true] when there isn't enough memory, [false] otherwise *)

  val check_low_memory_custom : int -> bool
  (** [check_low_memory_custom custom_words] is like {!val:check_low_memory},
      but optimized knowing that [custom_words] would get allocated until the
      next call to [check_low_memory_custom].

      This could be called from a Gc.Memprof callback, or at least on every
      network packet received. *)
end
