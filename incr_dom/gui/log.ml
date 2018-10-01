let error msg =
  let msg = "ERROR: " ^ msg in
  Async_js.log_s [%message msg]
