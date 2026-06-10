open Test_common
open Cmdliner

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

let tests =
  [
    ( "alloc_custom_bytes",
      allocator_test bytes_of_kib alloc_custom_bytes ignore_custom );
    ("alloc_words", allocator_test words_of_kib alloc_words ignore_ocaml);
  ]

let () = run_with_args "test_mem_limit" ulimit_v_kib tests
