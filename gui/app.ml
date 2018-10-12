let () =
  Incr_dom.Start_app.component (module Main) ~initial_model:(Main.init ())
