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
  ]

let () = run_with_args "test_mem_limit" ulimit_v_kib tests
