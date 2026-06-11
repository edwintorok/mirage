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
let limit = KiB (1 lsl 20) |> words_of_kib

(* this grows the heap more quickly by allocating arrays instead of lists *)
let rec grow_multiple count acc heap_words =
  if count <= 0 then Sys.opaque_identity acc
  else
    let acc = alloc_words (Words ctrl.minor_heap_size) :: acc in
    let heap_words' = gc_heap_words () in
    if heap_words' >= limit then (Alcotest.V1.skip [@tailcall]) ()
    else
      let count =
        if heap_words = heap_words' then count
        else begin
          let (Words heap_words) = heap_words
          and (Words heap_words') = heap_words' in
          Format.printf "heap_words=%d -> %d@." heap_words heap_words';
          count - 1
        end
      in
      (grow_multiple [@tailcall]) count acc heap_words

let with_grow_multiple count f =
  let data = grow_multiple count [] (gc_heap_words ()) in
  with_alive data f

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
    ( "major_heap_increment upper bound",
      List.init 30 @@ fun count ->
      gc_test_case (string_of_int count) @@ fun () ->
      with_grow_multiple count @@ fun () ->
      Gc.compact ();
      let (Words pre) = gc_heap_words () in
      (* must call this *before* growing the heap,
          because it might call [Gc.quick_stat ()] internally *)
      let actual =
        Private.major_heap_increment_words ctrl |> Sys.opaque_identity
      in
      let data = alloc_minor_heap (Words list_element_size) in
      Format.printf "data=%d words, minor_heap_size=%d words@."
        (Obj.reachable_words (Obj.repr data))
        ctrl.minor_heap_size;
      with_alive data @@ fun () ->
      Gc.full_major ();
      let (Words post) = gc_heap_words () in
      let expected = post - pre in
      Format.printf "heap_words:%d + %d = %d, increment=%f%%@." pre expected
        post
        (float_of_int expected /. float_of_int pre);
      (* we don't expect an exact match, just an upper bound in this test *)
      if actual < expected then
        check' int ~msg:"major_heap_increment_words lower bound" ~expected
          ~actual );
    ( "major_heap_increment overhead",
      chunk_sizes
      |> List.map @@ fun chunk_size ->
         gc_test_case (string_of_int chunk_size) @@ fun () ->
         if chunk_size - list_element_size = 1 then skip ()
         else
           let old = Gc.get () in
           let finally () = Gc.set old in
           Fun.protect ~finally @@ fun () ->
           (* to measure overhead more accurately on OCaml < 5 *)
           let ctrl = { old with major_heap_increment = 1024 } in
           Gc.set ctrl;
           Gc.compact ();
           (* grow heap once *)
           with_alive (alloc_minor_heap (Words list_element_size)) @@ fun () ->
           let data = minimize_free_words () in
           with_alive data @@ fun () ->
           let (Words pre) = gc_heap_words () in
           let actual =
             Private.major_heap_increment_words ctrl |> Sys.opaque_identity
           in
           let data = alloc_minor_heap (Words chunk_size) in
           with_alive data @@ fun () ->
           let (Words post) = gc_heap_words () in
           Format.printf "data: %d words@."
             (Obj.reachable_words (Obj.repr data));
           let expected = post - pre in
           Format.printf "%d -> %d + %d = %d; %.1f%%@." chunk_size pre expected
             post
             (100.
              *. float_of_int expected
              /. float_of_int ctrl.minor_heap_size
             -. 100.);
           (* we don't expect an exact match, just an upper bound in this test
              if this fails, then adjust overhead/overhead' in test_mem_grow.ml
            *)
           if actual < expected then
             check' int ~msg:"major_heap_increment_words grow" ~expected ~actual
    );
  ]

let () = run "test_mem_grow" tests
