type custom_heap =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type ocaml_heap = int array
type bytes = Bytes of int [@@unboxed]
type words = Words of int [@@unboxed]
type kib = KiB of int [@@unboxed]

let word_size_in_bytes = Sys.word_size / 8
let bytes_of_words (Words w) = Bytes (w * word_size_in_bytes)

let words_of_bytes (Bytes b) =
  Words ((b + word_size_in_bytes - 1) / word_size_in_bytes)

let bytes_of_kib (KiB kib) = Bytes (kib lsl 10)
let words_of_kib kib = kib |> bytes_of_kib |> words_of_bytes
let pp_kib ppf (KiB kib) = Format.fprintf ppf "%d KiB" kib

let alloc_custom_bytes (Bytes bytes) =
  Bigarray.(Array1.create char c_layout bytes)

let alloc_words (Words words) =
  if words = 0 then [||]
  else begin
    (* [Array.make 0 0] would return the
     empty array without allocating.
     So we cannot allocate exactly 1 word.
   *)
    assert (words > 1);
    (* the GC header uses 1 word *)
    Array.make (words - 1) 0
  end

let ignore_custom (ba : _ Bigarray.Array1.t) =
  ba |> Sys.opaque_identity |> ignore

let ignore_ocaml (a : _ array) = a |> Sys.opaque_identity |> ignore

let reset () =
  (* [Gc.live_words] doc says that [Gc.full_major ()]
     might have to be run twice, so always do that
     at the beginning of a test
   *)
  Gc.full_major ();
  Gc.compact ()

open Alcotest.V1

let gc_test_case name f =
  test_case name `Quick @@ fun arg ->
  reset ();
  f arg

let gc_heap_words () =
  (* must run either a minor or major cycle,
     otherwise the stats would be out-of-date (even [heap_words])
   *)
  Gc.minor ();
  Words Gc.(quick_stat ()).heap_words

let with_alive data f =
  let r = f () |> Sys.opaque_identity in
  let _keep = Sys.opaque_identity data in
  r

let full_major_stat () =
  Gc.full_major ();
  if Sys.ocaml_release.major < 5 then Gc.stat () else Gc.quick_stat ()

let list_element_size = 3

let rec minimize_free_words ctrl acc (Words last_heap_words) =
  let stat = full_major_stat () in
  let words =
    (100 * stat.largest_free / (100 + ctrl.Gc.space_overhead))
    - list_element_size
  in
  Format.printf "heap_words=%d->%d, free_words=%d, largest_free=%d@."
    last_heap_words stat.heap_words stat.free_words stat.largest_free;
  let stop_threshold = 32 in
  if stat.heap_words <> last_heap_words then
    failwith
      (Printf.sprintf
         "free_words didn't stabilize, adjust stopping threshold (currently \
          %d)!"
         stop_threshold);
  if words <= stop_threshold then acc
  else begin
    Format.printf "alloc_words(%d)@." words;
    let data = alloc_words (Words words) in
    let acc = data :: acc in
    (minimize_free_words [@tailcall]) ctrl acc (Words stat.heap_words)
  end

let minimize_free_words () =
  if Sys.ocaml_release.major >= 5 then
    (* OCaml 5.x allocates directly, not in increments *)
    []
  else minimize_free_words (Gc.get ()) [] (gc_heap_words ())
