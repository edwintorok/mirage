module Private = struct
  module Reservation = struct
    type t

    external map : int -> t = "stub_reservation_map_noalloc" [@@noalloc]

    external size_in_bytes : t -> int = "stub_reservation_size_in_bytes_noalloc"
    [@@noalloc]

    external unmap : t -> bool = "stub_reservation_unmap_noalloc" [@@noalloc]

    let empty = map 0
  end

  let try_alloc bytes =
    let open Reservation in
    let t = map bytes in
    let res = size_in_bytes t > 0 in
    res && unmap t

  let reservation = Atomic.make Reservation.empty

  let exchange_and_unmap_old t =
    Reservation.unmap (Atomic.exchange reservation t)

  let domain_count = 1 (* TODO *)
  let ctrl = Atomic.make (Gc.get ())
  let update_ctrl () = Atomic.set ctrl (Gc.get ())
  let (_ : Gc.alarm) = Gc.create_alarm update_ctrl

  let reserve_minor_heaps () =
    (* finalizers need 3 words for [struct final] for every 2 words in the minor heap,
       and there might also be some overhead when allocating small values
       (~11% on OCaml 5.x (see sizeclasses.h), and ~18%(measured) on OCaml 4.x, so round up to 20%) 
       1 + 3/2 + 1/5 = 27/10
      *)
    let bytes =
      domain_count * (Atomic.get ctrl).minor_heap_size * 27 * Sys.word_size / 80
    in
    let t = Reservation.map bytes in
    let ok = Reservation.size_in_bytes t > 0 in
    (* only replace the reservation on a successful allocation *)
    ok && exchange_and_unmap_old t

  let safe_compact () =
    (* Compaction has an implicit minor collection, which could crash
       with [caml_fatal_error] if we are out of memory.
       This can happen even if there'd be enough memory to free in the collection cycle.

       Free the reservation first to ensure the minor collection doesn't crash.
     *)
    let (_ : bool) = exchange_and_unmap_old Reservation.empty in
    Gc.compact ();
    (* may fail *)
    reserve_minor_heaps ()
end
