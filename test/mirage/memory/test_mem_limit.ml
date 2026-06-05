open Mirage_runtime_memory
open Test_common
open Cmdliner

let try_alloc_custom_bytes bytes =
  try
    alloc_custom_bytes (Bytes bytes) |> ignore_custom;
    true
  with Out_of_memory -> false

let kib =
  let parser str =
    str
    |> int_of_string_opt
    |> Option.map (fun i -> KiB i)
    |> Option.to_result ~none:"number expected"
  in
  Arg.Conv.make ~docv:"KiB" ~parser ~pp:pp_kib ()

let ulimit_v_kib =
  let doc = "virtual memory" in
  Arg.(required & opt (some kib) None & info [ "ulimit" ] ~doc ~docv:"KiB")

open Alcotest.V1

let allocator_test amount_of_kib alloc_amount ignore_allocated =
  let alloc_kib kib () =
    Format.printf "Allocating %a KiB@." pp_kib kib;
    kib |> amount_of_kib |> alloc_amount |> ignore_allocated
  in
  [
    gc_test_case "small" (fun _ -> alloc_kib (KiB 1) ());
    gc_test_case "large" (fun limit_kib ->
        check_raises "OOM" Out_of_memory @@ alloc_kib limit_kib);
  ]

let shifts = List.init 4 Fun.id
let packet_size = 1514
let cache = Stack.create ()

let on_low_memory () =
  let drop = Stack.length cache / 4 in
  for _ = 1 to drop do
    Stack.pop cache |> ignore
  done

let rec process_packets count =
  if count > 0 then begin
    let packet = alloc_custom_bytes (Bytes packet_size) in
    if count mod 100000 = 0 then
      Format.printf "process_packets remaining %d@." count;
    let count = count - 1 in
    if check_room_for_bytes packet_size then Stack.push packet cache
    else Format.printf "DROP@.";
    (process_packets [@tailcall]) count
  end

let tests =
  [
    ( "alloc_custom_bytes",
      allocator_test bytes_of_kib alloc_custom_bytes ignore_custom );
    ("alloc_words", allocator_test words_of_kib alloc_words ignore_ocaml);
    ( "try_alloc(small)",
      [
        gc_test_case "1" (fun _ ->
            check' bool ~msg:"should succeed" ~expected:true
              ~actual:(Private.try_alloc_bytes 1));
      ] );
    ( "try_alloc(large)",
      (* this is important to test, some C compilers
         might entirely optimize away the allocations from [try_alloc]
       *)
      [
        gc_test_case "limit" (fun limit_kib ->
            let (Bytes bytes) = limit_kib |> bytes_of_kib in
            check' bool ~msg:"should fail" ~expected:false
              ~actual:(Private.try_alloc_bytes bytes));
      ] );
    ( "try_alloc consistent",
      shifts
      |> List.map @@ fun shift ->
         gc_test_case (string_of_int shift) @@ fun limit_kib ->
         let (Bytes bytes) = limit_kib |> bytes_of_kib in
         let bytes = bytes lsr shift in
         let expected = Private.try_alloc_bytes bytes in
         let actual = Private.try_alloc_bytes bytes in
         check' bool ~msg:"consistent" ~expected ~actual );
    ( "try_alloc matches try_alloc_custom",
      shifts
      |> List.map @@ fun shift ->
         gc_test_case (string_of_int shift) @@ fun limit_kib ->
         let (Bytes bytes) = limit_kib |> bytes_of_kib in
         let bytes = bytes lsr shift in
         let actual = Private.try_alloc_bytes bytes in
         let expected = try_alloc_custom_bytes bytes in
         check' bool ~msg:"consistent" ~expected ~actual );
    ( "check_room_for_bytes",
      [
        (let count = 20 in
         gc_test_case (string_of_int count) @@ fun limit_kib ->
         let (Bytes bytes) = limit_kib |> bytes_of_kib in
         let bytes = bytes * count in
         (* attempt to allocate 10 * the memory limit
         drop allocations where [check_room_for_bytes]
         returns false
       *)
         let count = bytes / packet_size in
         register_on_low_memory on_low_memory;
         Format.printf "Simulating %d packets = %d bytes@." count bytes;
         process_packets count);
      ] );
  ]

let () = run_with_args "test_mem_limit" ulimit_v_kib tests
