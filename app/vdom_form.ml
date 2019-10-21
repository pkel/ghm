open Incr_dom.Vdom

type input =
  | Text
  | Number of { step : float }
  | Date
  | Int
  | Monetary

let rec attrs_of_type =
  let open Attr in
  function
  | Text -> [ type_ "text" ]
  | Number { step } -> [ type_ "number"; create "step" (string_of_float step) ]
  | Date -> [ type_ "date" ]
  | Int -> attrs_of_type (Number { step = 1. })
  | Monetary -> attrs_of_type (Number { step = 0.01 })
;;

let group = Node.div [ Attr.class_ "form-group" ]

let labelled_input ?(append = []) ?(prepend = []) =
  let cnt = ref 0 in
  fun ?(type_ = Text) label ->
    let id =
      incr cnt;
      !cnt
    in
    fun ~nth ?on_input value ->
      let id = Printf.sprintf "labelled_input_%i_%i" id nth in
      let input =
        Node.input
          ([ Attr.id id
           ; Attr.class_ "form-control"
           ; Attr.value value
           ; (match on_input with
             | Some on_input -> Attr.on_input (fun _ -> on_input)
             | None -> Attr.disabled)
           ]
          @ attrs_of_type type_)
          []
      in
      let input =
        match append, prepend with
        | [], [] -> input
        | _ ->
          Node.div
            [ Attr.class_ "input-group" ]
            (List.concat
               [ (match prepend with
                 | [] -> []
                 | l -> [ Node.div [ Attr.class_ "input-group-prepend" ] l ])
               ; [ Node.input
                     ([ Attr.id id
                      ; Attr.class_ "form-control"
                      ; Attr.value value
                      ; (match on_input with
                        | Some on_input -> Attr.on_input (fun _ -> on_input)
                        | None -> Attr.disabled)
                      ]
                     @ attrs_of_type type_)
                     []
                 ]
               ; (match append with
                 | [] -> []
                 | l -> [ Node.div [ Attr.class_ "input-group-append" ] l ])
               ])
      in
      group [ Node.label [ Attr.for_ id ] [ Node.text label ]; input ]
;;

let labelled_textfield =
  let cnt = ref 0 in
  fun ~rows label ->
    let id =
      incr cnt;
      !cnt
    in
    fun ~nth ?on_input value ->
      let id = Printf.sprintf "labelled_input_%i_%i" id nth in
      group
        [ Node.label [ Attr.for_ id ] [ Node.text label ]
        ; Node.textarea
            [ Attr.id id
            ; Attr.class_ "form-control"
            ; Attr.create "rows" (string_of_int rows)
            ; (match on_input with
              | Some on_input -> Attr.on_input (fun _ -> on_input)
              | None -> Attr.disabled)
            ]
            [ Node.text value ]
        ]
;;
