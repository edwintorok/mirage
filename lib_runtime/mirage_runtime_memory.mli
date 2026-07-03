(**/**)
module Private: sig
    module Reservation: sig
        type t

        val size_in_bytes : t -> int
        (** [size_in_bytes t] is the amount of bytes reserved.

            @returns -1 if the reservation failed
         *)

        val map : int -> t
        (** [map bytes] reserves [bytes] using [mmap(2)].
            Does not raise exceptions, and doesn't allocate OCaml values.
            Use {!val:size_in_bytes} to check whether the reservation succeeded.
        *)

        val unmap: t -> bool
        (** [unmap reservation] frees memory associated with [reservation].
            This is safe to call on failed reservations (it is a no-op and returns true).
            Do not call this multiple times with the same [reservation] (double-free).

            @returns [true] if unmapping succeeded, [false] if [munmap(2)] fails.
        *)
    end

    val try_alloc: int -> bool
    (** [try_alloc bytes] attempts to allocate and immediately free [bytes].
        Doesn't raise exceptions, and doesn't allocate OCaml values.
    
        @returns [true] if the allocation succeeded
    *)
end
