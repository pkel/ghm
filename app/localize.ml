open Core_kernel

let locale = "de-DE"
let date d = Browser.Date.(of_string (Date.to_string d) |> to_locale_date_string_ ~locale)
