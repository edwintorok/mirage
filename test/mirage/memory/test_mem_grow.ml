open Test_common
open Mirage_runtime_memory
open Alcotest.V1

let ctrl = Gc.get ()
let list_element_size = 3

type 'a t = N | S of 'a t | L of 'a * 'a t

let rec init_s count acc =
  if count <= 0 then acc else (init_s [@tailcall]) (count - 1) (S acc)

let init_s _ count = init_s count N

let rec init_l chunk count acc =
  if count <= 0 then acc
  else
    let data = alloc_words chunk in
    (init_l [@tailcall]) chunk (count - 1) (L (data, acc))

let init_l chunk count = init_l chunk count N

(** [alloc_minor_heap chunk_size] allocates an entire minor heap that consists
    of elements of [chunk_size] words.

    This explores sizeclass pool allocation overheads *)
let alloc_minor_heap (Words chunk_size) =
  let element_size, init =
    if chunk_size = 2 then (2, init_s) else (3, init_l)
  in
  let count = ctrl.minor_heap_size / chunk_size in
  let chunk_size = chunk_size - element_size in
  Format.printf "alloc_minor_heap: %d x (%d + alloc_words %d)@." count
    element_size chunk_size;
  Gc.minor ();
  init (Words chunk_size) count

let max_young_wosize = 256
let chunk_sizes = List.init (max_young_wosize - 1) (fun x -> x + 2)

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
          (* grow heap twice, avoids corner cases on small heaps *)
          with_alive (alloc_minor_heap (Words list_element_size)) @@ fun () ->
          with_alive (alloc_minor_heap (Words list_element_size)) @@ fun () ->
          Gc.compact ();
          (* call the function under test first, it may call Gc.quick_stat,
            and that would look at the current heap size.
          *)
          let actual = Private.increment_of_heap ctrl list_element_size in
          if actual = 0 then skip ()
          else
            let Words heap_words, Words heap_words' = grow_once () in
            let expected = heap_words' - heap_words in
            Format.printf "%d + %d = %d@." heap_words expected heap_words';
            check' int ~msg:"increment_of_heap" ~expected ~actual );
      ] );
  ]

let () = run "test_mem_grow" tests
