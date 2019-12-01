let int =
  let i = ref 0 in
  function
  | () ->
    incr i;
    !i
;;
