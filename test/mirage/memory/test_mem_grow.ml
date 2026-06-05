open Test_common
open Mirage_runtime_memory
open Alcotest.V1

let ctrl = Gc.get ()

(** [alloc_minor_heap chunk_size] allocates an entire minor heap that consists
    of elements of [chunk_size] words.

    This explores sizeclass pool allocation overheads *)
let alloc_minor_heap (Words chunk_size) =
  let count = (ctrl.minor_heap_size + chunk_size - 1) / chunk_size in
  let chunk_size = chunk_size - 2 in
  Format.printf "%d x (1 + alloc_words %d)@." count chunk_size;
  List.init count @@ fun _ -> alloc_words (Words chunk_size)

let max_young_wosize = 256
let chunk_sizes = List.init (max_young_wosize - 3) (fun x -> x + 3)

let rec grow_once heap_words acc =
  let acc = () :: acc |> Sys.opaque_identity in
  let heap_words' = gc_heap_words () in
  if heap_words = heap_words' then (grow_once [@tailcall]) heap_words acc
  else (heap_words, heap_words')

let grow_once () = grow_once (gc_heap_words ()) []

let tests =
  [
    ( "increment_of_heap",
      [
        ( gc_test_case "once" @@ fun () ->
          (* grow heap once *)
          with_alive (alloc_minor_heap (Words 2)) @@ fun () ->
          (* call the function under test first, it may call Gc.quick_stat,
            and that would look at the current heap size.
          *)
          let actual = Private.increment_of_heap ctrl 2 in
          if actual = 0 then skip ()
          else
            let Words heap_words, Words heap_words' = grow_once () in
            let expected = heap_words' - heap_words in
            Format.printf "%d + %d = %d@." heap_words expected heap_words';
            check' int ~msg:"increment_of_heap" ~expected ~actual );
      ] );
  ]

let () = run "test_mem_grow" tests
