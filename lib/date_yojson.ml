include Core_kernel.Date

let to_yojson t =
  [%to_yojson: string] (to_string t)

let of_yojson s =
  match [%of_yojson: string] s with
  | Error s -> Error s
  | Ok t -> Ok (of_string t)
