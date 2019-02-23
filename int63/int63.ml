open Base

let () = Stdio.print_endline Int63.(zero |> to_int_exn |> Int.to_string)
