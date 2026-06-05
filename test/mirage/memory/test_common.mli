type custom_heap
(** shorter type for bigarrays (custom allocations) *)

type ocaml_heap
(** OCaml heap allocation *)

(** {1 Units for memory allocations.}

    To avoid accidentally mixing bytes, words and KiB (they're all integers)
    introduce a separate type for each. Mark them unboxed to avoid runtime
    overhead. *)

type bytes = Bytes of int [@@unboxed]

(** {!val:Sys.word_size} / 8 bytes *)
type words = Words of int [@@unboxed]

(** 1024 {!type:bytes} *)
type kib = KiB of int [@@unboxed]

val pp_kib : Format.formatter -> kib -> unit
(** [pp_kib kib] prints [kib] in a human readable format. *)

val bytes_of_words : words -> bytes
(** [bytes_of_words w] is [w * Sys.word_size / 8]. *)

val words_of_bytes : bytes -> words
(** [words_of_bytes b] ensures that [w |> bytes_of_words |> words_of_bytes = w].
*)

val bytes_of_kib : kib -> bytes
(** [bytes_of_kib kib] is [kib * 1024] *)

val words_of_kib : kib -> words
(** [words_of_kib kib] is [kib |> bytes_of_kib |> words_of_bytes] *)

(** {1 Allocation helpers} *)

val alloc_custom_bytes : bytes -> custom_heap
(** [alloc_custom_bytes bytes] allocates [bytes] of {!type:custom} memory. This
    also allocates an OCaml value (the bigarray), so the total memory required
    is larger than [bytes]. *)

val alloc_words : words -> ocaml_heap
(** [alloc_words words] allocates [words] on the OCaml heap. [words] cannot be
    [1], and must be [>= 0]. *)

val ignore_custom : custom_heap -> unit
(** [ignore_custom custom] is a convenience function for ignoring a
    {!type:custom_heap} value. Better than {!val:ignore}, doesn't ignore partial
    applications. *)

val ignore_ocaml : ocaml_heap -> unit
(** [ignore_ocaml custom] is a convenience function for ignoring an
    {!type:ocaml_heap} value. Better than {!val:ignore}, doesn't ignore partial
    applications. *)

val gc_test_case : string -> ('a -> unit) -> 'a Alcotest.V1.test_case
(** [gc_test_case name f] is the testcase [f] with [name]. It runs the garbage
    collector to compact the heap before each test. *)

val gc_heap_words : unit -> words
(** [gc_heap_words ()] is the size of the OCaml major heap in words *)

val with_alive : 'a -> (unit -> 'b) -> 'b
(** [with_alive data f] calls [f], keeping [data] alive across the call. *)
