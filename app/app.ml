let () =
  Incr_dom.Start_app.start
    (module Main)
    ~bind_to_element_with_id:"ghm_main"
    ~initial_model:(Main.init ())
;;
