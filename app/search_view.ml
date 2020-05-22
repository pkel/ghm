open Core_kernel
open Incr_dom
module Incr_map = Incr_map.Make (Incr)

let page_size = 250

module Model = struct
  type t =
    { customers : [ `Ok of Pg.Customers.return Int.Map.t | `Loading | `Failed ]
    ; table : Customer_table.Model.t
    ; search_init : string
    ; search_input : string
    ; fts : bool
    ; page : int
    }
  [@@deriving compare, fields]

  let create () =
    { customers = `Loading
    ; table = Customer_table.Model.create ()
    ; search_init = ""
    ; search_input = ""
    ; fts = false
    ; page = 0
    }
  ;;
end

module Action = struct
  type t =
    | Fts of bool
    | Search
    | Search_input of string
    | Search_reset
    | Get_more
    | Table of Customer_table.Action.t
    | Got_customers of
        { page : int
        ; resp : Pg.Customers.return list sexp_opaque Or_error.t
        }
  [@@deriving sexp_of, variants]

  let refresh = Search
end

let get_customers ~conn ~schedule_action ?(page = 0) ?filter () =
  let offset = if page > 0 then Some ((page * page_size) + 1) else None in
  Xhr.send'
    ~c:conn
    Pg.(
      read ~order:[ desc Customers.modified ] ?offset ~limit:page_size ?filter Customers.t)
    ~handler:(fun resp -> schedule_action Action.(got_customers ~page ~resp))
;;

let fts_filter_of_input s =
  let open String in
  let s = strip s in
  match s with
  | "" -> None
  | s ->
    let open Pg.Tsvector in
    Some Pg.(websearch Customers.tsv s)
;;

let keyword_filter_of_input s =
  let open String in
  let s = strip s in
  let l, r = is_prefix ~prefix:"_" s, is_suffix ~suffix:"_" s in
  match
    strip
      ~drop:(function
        | '_' -> true
        | _ -> false)
      s
  with
  | "" -> None
  | s ->
    let s = (if l then "" else "%") ^ s ^ if r then "" else "%" in
    let open Pg.String in
    Some Pg.(ilike Customers.keyword s)
;;

let apply_action
    ~table
    (model : Model.t)
    (action : Action.t)
    (state : State.t)
    ~schedule_action
  =
  match action with
  | Got_customers { resp = Error detail; _ } ->
    state.handle_error { gist = "Laden von Kunden fehlgeschlagen"; detail };
    model
  | Got_customers { page; resp = Ok l } ->
    let customers =
      List.mapi l ~f:(fun i x -> i + (page * page_size), x)
      |> Int.Map.of_alist_or_error
      |> function
      | Error detail ->
        state.handle_error { gist = "Laden von Kunden fehlgeschlagen"; detail };
        model.customers
      | Ok m ->
        if page > 0
        then (
          let old =
            match model.customers with
            | `Ok x -> x
            | _ -> Int.Map.empty
          in
          match Int.Map.append ~lower_part:old ~upper_part:m with
          | `Ok m -> `Ok m
          | `Overlapping_key_ranges ->
            state.handle_error
              { gist = "Laden von Kunden fehlgeschlagen"
              ; detail = Error.of_string "Overlapping key ranges"
              };
            model.customers)
        else `Ok m
    in
    { model with customers; page }
  | Table a ->
    let schedule_action = Fn.compose schedule_action Action.table in
    let table = Component.apply_action ~schedule_action table a () in
    { model with table }
  | Fts fts ->
    schedule_action Search;
    { model with fts }
  | Search ->
    let filter =
      if model.fts
      then fts_filter_of_input model.search_input
      else keyword_filter_of_input model.search_input
    in
    get_customers ~conn:state.connection ~schedule_action ?filter ();
    { model with search_init = model.search_input; customers = `Loading }
  | Search_input search_input -> { model with search_input }
  | Get_more ->
    let page = model.page + 1
    and filter =
      if model.fts
      then fts_filter_of_input model.search_input
      else keyword_filter_of_input model.search_input
    in
    get_customers ~page ~conn:state.connection ~schedule_action ?filter ();
    model
  | Search_reset ->
    get_customers ~conn:state.connection ~schedule_action ();
    { model with search_init = ""; search_input = ""; fts = false }
;;

open Vdom
open Incr.Let_syntax

let view_search ~inject ~init ~fts =
  let%bind init = init
  and fts = fts in
  let%map input =
    let prepend =
      [ Bs.button
          (Icon (S "undo", "Zurücksetzen"))
          (Action (fun () -> inject Action.search_reset))
      ]
    and append = [ Bs.button (Text "Suchen") Submit ]
    and placeholder = if fts then "Suchwort" else "Schlüsselwort" in
    Incr_dom_widgets.Interactive.render
      ~on_input:Action.search_input
      ~inject
      (Bs.Form.string ~prepend ~append ~placeholder ~init ())
  and fts =
    Incr_dom_widgets.Interactive.render
      ~on_input:Action.fts
      ~inject
      (Bs.Form.checkbox ~init:fts ~label:"Volltextsuche" ())
  in
  Node.create
    "form"
    [ Attr.on "submit" (fun _ -> inject Action.Search)
    ; Attr.classes [ "mb-2"; "mt-2"; "form-inline" ]
    ]
    [ input; Node.span [ Attr.style Css_gen.(width (`Em 1)) ] []; fts ]
;;

let view ~inject ~table (model : Model.t Incr.t) =
  let more =
    let open Bs.Grid in
    [ row
        ~c:[ "justify-content-center" ]
        [ col_auto
            ~c:[ "mb-2" ]
            [ Bs.button (Text "Mehr") (Action (fun _ -> inject Action.get_more)) ]
        ]
    ]
  in
  let%map head =
    view_search ~fts:(model >>| Model.fts) ~inject ~init:(model >>| Model.search_init)
  and customers = model >>| Model.customers
  and page = model >>| Model.page
  and table = table in
  let nodes =
    head
    ::
    (match customers with
    | `Ok m when Int.Map.length m = (page + 1) * page_size -> Component.view table :: more
    | `Ok _ -> [ Component.view table ]
    | `Loading ->
      [ Bs.Grid.loading_row ] (* TODO: create and use Bs.Grid.failed_row for `Failed ?*)
    | _ -> [])
  in
  Node.div [ Attr.on "scroll" (fun _ -> Event.Viewport_changed) ] nodes
;;

let create ~old_model ~inject model =
  let customers = model >>| Model.customers in
  let table =
    let model = model >>| Model.table
    and old_model = old_model >>| Model.table
    and inject = Fn.compose inject Action.table
    and rows =
      let customers =
        customers
        >>| function
        | `Ok x -> x
        | _ -> Int.Map.empty
      in
      Incr_map.mapi customers ~f:(fun ~key:_ ~(data : Pg.Customers.return) ->
          Customer_table.Model.Row.of_customer ~id:data.id data)
    in
    Customer_table.create rows ~old_model ~inject ~model
  in
  let%map model = model
  and table = table
  and view = view ~inject ~table model in
  let apply_action = apply_action ~table model
  and update_visibility ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.table in
    let table = Component.update_visibility table ~schedule_action in
    { model with table }
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.table in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~apply_action ~update_visibility ~on_display model view
;;
