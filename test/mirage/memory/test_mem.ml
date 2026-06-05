open Alcotest
open Mirage_runtime_memory

let word_size_in_bytes = Sys.word_size / 8

let try_alloc_custom_bytes bytes =
  try
    let (_ : _ Bigarray.Array1.t) =
      Bigarray.(Array1.create char c_layout bytes)
    in
    Gc.full_major ();
    true
  with Out_of_memory -> false

(* set with setrlimit *)
let limit_log2 = 30

(* at least one test beyond limit *)
let test_limit_log2 = limit_log2 + 1

let sizes_log2 =
  let n = 4 in
  List.init n (fun i -> test_limit_log2 + i - (n - 1))

let size_test size_log2 f =
  let name = Printf.sprintf "2^%d" size_log2 in
  V1.test_case name `Quick @@ fun () -> f (1 lsl size_log2)

let tests =
  [
    ( "try_alloc(small)",
      [
        ( size_test 1 @@ fun size ->
          V1.(
            check' bool ~msg:"should succeed" ~expected:true
              ~actual:(Private.try_alloc_bytes size)) );
      ] );
    ( "try_alloc(large)",
      (* this is important to test, some C compilers
         might entirely optimize away the allocations from [try_alloc]
       *)
      [
        ( size_test (limit_log2 + 1) @@ fun size ->
          V1.(
            check' bool ~msg:"should fail" ~expected:false
              ~actual:(Private.try_alloc_bytes size)) );
      ] );
    ( "try_alloc consistent",
      sizes_log2
      |> List.map @@ fun size_log2 ->
         size_test size_log2 @@ fun size ->
         let expected = Private.try_alloc_bytes size in
         let actual = Private.try_alloc_bytes size in
         check' bool ~msg:"consistent" ~expected ~actual );
    ( "try_alloc matches try_alloc_custom",
      sizes_log2
      |> List.map @@ fun size_log2 ->
         size_test size_log2 @@ fun size ->
         let actual = Private.try_alloc_bytes size in
         let expected = try_alloc_custom_bytes size in
         check' bool ~msg:"matches" ~expected ~actual );
  ]

let () =
  Printf.printf "OCAMLRUNPARAM=%s\n"
    (Sys.getenv_opt "OCAMLRUNPARAM" |> Option.value ~default:"");
  V1.run "test_mem" tests
