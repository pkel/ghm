open Core_kernel

let locale = "de-DE"

let month m =
  match Month.to_int m with
  | 1 -> "Januar"
  | 2 -> "Februar"
  | 3 -> "März"
  | 4 -> "April"
  | 5 -> "Mai"
  | 6 -> "Juni"
  | 7 -> "Juli"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "Oktober"
  | 11 -> "November"
  | 12 -> "Dezember"
  | _ -> "ungültiger Monat"
;;

let date d =
  Browser.Date.(of_string (Date.to_string d) |> to_locale_date_string_ ~locale)
;;
