module Private = struct
  let[@inline] round_up n ~multiple_of =
    (n + multiple_of - 1) / multiple_of * multiple_of

  let[@inline] round_up_page_size w = round_up w ~multiple_of:512

  let rec increment_percentage ~percentage ~requested_heap_words ~heap_words =
    if heap_words < requested_heap_words then
      let heap_words =
        round_up_page_size @@ (heap_words + (heap_words / 100 * percentage))
      in
      (increment_percentage [@tailcall]) ~percentage ~requested_heap_words
        ~heap_words
    else heap_words

  let[@inline] increment_of_heap ctrl requested_words =
    match ctrl.Gc.major_heap_increment with
    | 0 ->
        (* only possible value on OCaml 5.x,
           as an optimization we don't need to call [Gc.quick_stat].
           when we grow the heap we always grow it by at least one page.
         *)
        0
    | percentage when percentage <= 1000 ->
        (* increments <= 1000 are percentages *)
        let heap_words = Gc.(quick_stat ()).heap_words in
        let requested_heap_words = heap_words + requested_words in
        let heap_words' =
          increment_percentage ~percentage ~requested_heap_words ~heap_words
        in
        heap_words' - heap_words
    | words ->
        (* increments > 1000 are fixed number of words *)
        assert (words > 0);
        round_up requested_words ~multiple_of:(round_up_page_size words)
end
