open Base

type action =
  | Delete
  | Duplicate
  | Move_down
  | Move_up
[@@deriving sexp]

let pop nth l =
  let rec delete acc = function
    | 0, hd :: tl -> List.rev_append acc tl, Some hd
    | _, [] -> l, None
    | nth, hd :: tl -> delete (hd :: acc) (nth - 1, tl)
  in
  if nth < 0 then l, None else delete [] (nth, l)
;;

let insert at a l =
  let rec add acc = function
    | 0, tl -> List.rev_append acc (a :: tl)
    | _, [] -> List.rev (a :: acc)
    | at, hd :: tl -> add (hd :: acc) (at - 1, tl)
  in
  if at < 0 then a :: l else add [] (at, l)
;;

let move ~nth ~by l =
  match pop nth l with
  | l, Some a ->
    let at = nth + by in
    insert at a l
  | _ -> l
;;

let apply_action l nth = function
  | Delete -> fst (pop nth l)
  | Duplicate ->
    let rec dup acc = function
      | 0, hd :: tl -> List.rev_append acc (hd :: hd :: tl)
      | _, [] -> l
      | i, hd :: tl -> dup (hd :: acc) (i - 1, tl)
    in
    if nth < 0 then l else dup [] (nth, l)
  | Move_down -> move ~nth ~by:1 l
  | Move_up -> move ~nth ~by:(-1) l
;;

let view ~inject descriptor =
  let a label icon a =
    let label = descriptor ^ " " ^ label
    and f () = inject a in
    Bs.button ~tabskip:true ~size:`Small ~color:`Light (Icon (icon, label)) (Action f)
  in
  [ a "nach unten verschieben" (S "chevron-down") Move_down
  ; a "nach oben verschieben" (S "chevron-up") Move_up
  ; a "duplizieren" (R "clone") Duplicate
  ; a "löschen" (S "trash") Delete
  ]
;;
