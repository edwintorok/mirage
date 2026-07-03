module Private = struct
  module Reservation = struct
    type t

    external map : int -> t = "stub_reservation_map_noalloc" [@@noalloc]

    external size_in_bytes : t -> int = "stub_reservation_size_in_bytes_noalloc" [@@noalloc]

    external unmap: t -> bool = "stub_reservation_unmap_noalloc" [@@noalloc]

  end

  let try_alloc bytes =
    let open Reservation in
    let t = map bytes in
    let res = size_in_bytes t > 0 in
    res && unmap t
end
