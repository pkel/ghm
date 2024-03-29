open Incr_dom.Vdom
open Core_kernel

type icon =
  | B of string
  | R of string
  | S of string
  | L of string

let classes_of_icon = function
  | B s -> [ "fab"; sprintf "fa-%s" s ]
  | R s -> [ "far"; sprintf "fa-%s" s ]
  | S s -> [ "fas"; sprintf "fa-%s" s ]
  | L s -> [ "fal"; sprintf "fa-%s" s ]
;;

let icon ?(c = []) i = Node.create "i" [ Attr.classes (classes_of_icon i @ c) ] []

type button_action =
  | Href of string
  | Href_blank of string
  | Action of (unit -> Event.t)
  | Clipboard of string
  | Download of
      { filename : string
      ; media_type : string
      ; content : string
      }
  | Download_b64 of
      { filename : string
      ; media_type : string
      ; b64 : string
      }
  | Submit
  | No_action

type button_view =
  | Icon of (icon * string)
  | Text of string

(* | Generic of {title: string option; children: Node.t list} *)

let button
    ?(tabskip = false)
    ?(disabled = false)
    ?(color = `Secondary)
    ?(size = `Normal)
    view
    action
  =
  let cls =
    let h s = Some ("btn-" ^ s) in
    let color =
      match color with
      | `None -> None
      | `Primary -> h "primary"
      | `Secondary -> h "secondary"
      | `Light -> h "light"
      | `Outline_danger -> h "outline-danger"
      | `Outline_warning -> h "outline-warning"
      | `Outline_success -> h "outline-success"
    and size =
      match size with
      | `Normal -> None
      | `Small -> h "sm"
      | `Large -> h "lg"
      | `Block -> h "block"
    in
    List.filter_opt [ color; size; Some "btn" ]
  and attrs =
    List.filter_opt
      [ (if disabled then Some Attr.disabled else None)
      ; (if tabskip then Some (Attr.create "tabindex" "-1") else None)
      ]
  and node, action_attrs, action_cls =
    match action with
    | Href s -> Node.a, Attr.[ href s; create "role" "button" ], []
    | Href_blank s ->
      ( Node.a
      , Attr.
          [ href s
          ; create "role" "button"
          ; create "target" "blank"
          ; create "rel" "noopener norefferer"
          ]
      , [] )
    | Action f -> Node.button, Attr.[ on_click (fun _ -> f ()); type_ "button" ], []
    | No_action -> Node.button, [ Attr.type_ "button" ], []
    | Submit -> Node.button, [ Attr.type_ "submit" ], []
    | Download_b64 f ->
      ( Node.a
      , Attr.
          [ href (sprintf "data:%s;base64,%s" f.media_type f.b64)
          ; create "download" f.filename
          ; create "role" "button"
          ]
      , [] )
    | Download f ->
      ( Node.a
      , Attr.
          [ href
              (sprintf "data:%s;base64,%s" f.media_type (Base64.encode_string f.content))
          ; create "download" f.filename
          ; create "role" "button"
          ]
      , [] )
    | Clipboard value ->
      let id = Base.(sprintf "clipboard-%i" (Hash.Builtin.hash_string value)) in
      let node ?key attr children =
        Node.div
          []
          [ Node.button ?key (Attr.type_ "button" :: attr) children
          ; Node.textarea
              [ Attr.id id
              ; Attr.style
                  Css_gen.(
                    concat
                      [ opacity 0.
                      ; position ~left:(`Px 0) ~top:(`Px 0) `Absolute
                      ; z_index (-1)
                      ; height (`Px 0)
                      ; width (`Px 0)
                      ; overflow `Hidden
                      ; border ~style:`None ()
                      ])
              ]
              [ Node.text value ]
          ]
      and attrs = [ Attr.create "data-clipboard-target" ("#" ^ id) ]
      and cls = [ "clipboard-js" ] in
      node, attrs, cls
  and content, view_attrs =
    match view with
    | Icon (i, label) -> [ icon i ], [ Attr.create "title" label ]
    | Text s -> [ Node.text s ], []
  in
  let attrs = (Attr.classes (action_cls @ cls) :: attrs) @ action_attrs @ view_attrs in
  node attrs content
;;

module Grid = struct
  module A = Attr
  module N = Node
  open N
  open A

  let div = div
  let row ?(c = []) = div [ classes ("row" :: c) ]
  let frow ?(c = []) = div [ classes ("form-row" :: c) ]
  let col ?(c = []) = div [ classes ("col" :: c) ]
  let col1 ?(c = []) = div [ classes ("col-1" :: c) ]
  let col2 ?(c = []) = div [ classes ("col-2" :: c) ]
  let col3 ?(c = []) = div [ classes ("col-3" :: c) ]
  let col4 ?(c = []) = div [ classes ("col-4" :: c) ]
  let col5 ?(c = []) = div [ classes ("col-5" :: c) ]
  let col6 ?(c = []) = div [ classes ("col-6" :: c) ]
  let col7 ?(c = []) = div [ classes ("col-7" :: c) ]
  let col8 ?(c = []) = div [ classes ("col-8" :: c) ]
  let col9 ?(c = []) = div [ classes ("col-9" :: c) ]
  let col10 ?(c = []) = div [ classes ("col-10" :: c) ]
  let col11 ?(c = []) = div [ classes ("col-11" :: c) ]
  let col_auto ?(c = []) = div [ classes ("col-auto" :: c) ]

  let loading_row =
    row
      ~c:[ "justify-content-center"; "align-items-center"; "pt-5" ]
      [ col_auto
          [ div
              [ Attr.class_ "spinner-border"; Attr.create "role" "status" ]
              [ span [ Attr.class_ "sr-only" ] [ Node.text "Lädt..." ] ]
          ]
      ]
  ;;
end

module Form = struct
  open Ghm
  open Incr_dom
  open Vdom
  open Incr_dom_widgets
  open Interactive

  module Primitives = struct
    let shared_setup =
      let incr =
        let counter = ref 0 in
        fun () ->
          incr counter;
          "ghm_form_" ^ Int.to_string !counter
      in
      fun ~id ->
        let key = incr () in
        key, Option.value id ~default:key
    ;;

    let generic
        (type a)
        ~(of_string : string -> (a, string) result)
        ~(to_string : a -> string)
        ~init
        ~render
        ?id
        ()
        : a t
      =
      let open Incr.Let_syntax in
      let key, id = shared_setup ~id in
      Primitives.create ~init ~render:(fun ~inject ~value ->
          let%map value = value in
          let on_input =
            Attr.on_input (fun _ev text ->
                match of_string text with
                | Ok x -> inject x
                | Error _ -> Event.Ignore)
          in
          let attrs = List.filter_opt [ Some (Attr.id id); Some on_input ]
          and value = to_string value in
          render ~key ~id ~attrs ~value)
    ;;
  end

  type input =
    | Text
    | Number of { step : float }
    | Date
    | Int

  let rec attrs_of_type =
    let open Attr in
    function
    | Text -> [ type_ "text" ]
    | Number { step } -> [ type_ "number"; create "step" (string_of_float step) ]
    | Date -> [ type_ "date" ]
    | Int -> attrs_of_type (Number { step = 1. })
  ;;

  let checkbox ~init ?label ?(disabled = false) () =
    let _key, id = Primitives.shared_setup ~id:None in
    let open Incr_dom_widgets.Interactive in
    let checkbox =
      let attrs =
        Attr.class_ "form-check-input"
        :: (if disabled then [ Attr.create "disabled" "true" ] else [])
      in
      Primitives.checkbox ~attrs ~id ~init ()
    in
    map_nodes checkbox ~f:(fun nodes ->
        [ Node.div
            [ Attr.classes [ "form-group"; "form-check" ] ]
            (nodes
            @
            match label with
            | Some s ->
              [ Node.label
                  [ Attr.class_ "form-check-label"; Attr.for_ id ]
                  [ Node.text s ]
              ]
            | None -> [])
        ])
  ;;

  let input_conv
      (type a)
      ?(type_ = Text)
      ~(of_string : string -> (a, string) result)
      ~(to_string : a -> string)
      ~init
      ?(prepend = [])
      ?(append = [])
      ?datalist
      ?placeholder
      ?label
      ?(disabled = false)
      ()
      : a t
    =
    let label = Option.map ~f:(fun l -> Node.label [] [ Node.text l ]) label in
    let err msg = Node.div [ Attr.class_ "invalid-feedback" ] [ Node.text msg ] in
    let group input =
      match append, prepend with
      | [], [] -> input
      | _ ->
        Node.div
          [ Attr.class_ "input-group" ]
          (List.concat
             [ (match prepend with
               | [] -> []
               | l -> [ Node.div [ Attr.class_ "input-group-prepend" ] l ])
             ; [ input ]
             ; (match append with
               | [] -> []
               | l -> [ Node.div [ Attr.class_ "input-group-append" ] l ])
             ])
    in
    let render ~key ~id ~attrs ~value =
      let dl_id, dl =
        match datalist with
        | Some l ->
          let id = id ^ "_datalist" in
          ( Some id
          , Some
              (Node.create
                 "datalist"
                 [ Attr.id id ]
                 (List.map l ~f:(fun x -> Node.option [ Attr.value x ] []))) )
        | None -> None, None
      in
      let attrs classes =
        List.filter_opt
          [ Some (Attr.classes ("form-control" :: classes))
          ; Some (Attr.type_ "text")
          ; Some (Attr.value value)
          ; (if disabled then Some (Attr.create "disabled" "true") else None)
          ; Option.map dl_id ~f:(Attr.create "list")
          ; Option.map placeholder ~f:Attr.placeholder
          ]
        @ attrs_of_type type_
        @ attrs
      in
      let nodes =
        match of_string value with
        | Ok _ -> [ label; Some (Node.input ~key (attrs []) [] |> group); dl ]
        | Error msg ->
          [ label
          ; Some (Node.input ~key (attrs [ "is-invalid" ]) [] |> group)
          ; Some (err msg)
          ; dl
          ]
      in
      [ Node.div [ Attr.class_ "form-group" ] (List.filter_opt nodes) ]
    in
    Primitives.generic ~to_string ~of_string ~init ~render ()
  ;;

  (* TODO: Make this similar to string input field (ideally reuse) *)
  let textarea ?(validator = fun _ -> None) ~nrows ~init ?placeholder ?label () =
    let label = Option.map ~f:(fun l -> Node.label [] [ Node.text l ]) label in
    let err msg = Node.div [ Attr.class_ "invalid-feedback" ] [ Node.text msg ] in
    let render ~key ~id:_ ~attrs ~value =
      let attrs classes =
        let tl =
          Attr.classes ("form-control" :: classes)
          :: Attr.create "rows" (Int.to_string nrows)
          :: attrs
        in
        match placeholder with
        | None -> tl
        | Some p -> Attr.placeholder p :: tl
      in
      let nodes =
        match validator value with
        | None -> [ label; Some (Node.textarea ~key (attrs []) [ Node.text value ]) ]
        | Some msg ->
          [ label
          ; Some (Node.textarea ~key (attrs [ "is-invalid" ]) [ Node.text value ])
          ; Some (err msg)
          ]
      in
      [ Node.div [ Attr.class_ "form-group" ] (List.filter_opt nodes) ]
    in
    Primitives.generic ~to_string:Fn.id ~of_string:Result.return ~render ~init ()
  ;;

  let opt ~of_string ~to_string =
    let of_string s =
      match String.strip s with
      | "" -> Ok None
      | s -> Result.map ~f:Option.return (of_string s)
    and to_string x = Option.map ~f:to_string x |> Option.value ~default:"" in
    input_conv ~to_string ~of_string
  ;;

  let monetary, monetary_opt =
    let type_ = Number { step = 0.01 }
    and of_string s =
      let e = Result.failf "Interner Fehler: ungültiger Geldwert: %s" s in
      match Float.of_string (String.strip s) |> Monetary.of_float with
      | Some m -> Ok m
      | None -> e
      | exception _ -> e
    and to_string = Monetary.to_string_dot in
    input_conv ~to_string ~of_string ~type_, opt ~to_string ~of_string ~type_
  ;;

  let date, date_opt =
    let type_ = Date
    and of_string s =
      match Date.of_string (String.strip s) with
      | x -> Ok x
      | exception _ -> Result.failf "Interner Fehler: Ungültiges Datum: %s" s
    and to_string = Date.to_string in
    input_conv ~to_string ~of_string ~type_, opt ~to_string ~of_string ~type_
  ;;

  let int, int_opt =
    let type_ = Int
    and of_string s =
      match Int.of_string (String.strip s) with
      | x -> Ok x
      | exception _ -> Result.failf "Interner Fehler: Ungültige Zahl: %s" s
    and to_string = Int.to_string in
    input_conv ~to_string ~of_string ~type_, opt ~to_string ~of_string ~type_
  ;;

  let string, string_opt =
    let of_string s = Ok (String.strip s)
    and to_string = Fn.id in
    input_conv ~to_string ~of_string, opt ~to_string ~of_string
  ;;
end
