open Core_kernel
open Ghm
open Incr_dom
module Incr_map = Incr_map.Make (Incr)
module Form = Incr_dom_widgets.Form
module State = State

module Model = struct
  (* TODO: make this Customer.with_id? *)
  type customer =
    { id : int
    ; data : Customer.t }
  [@@deriving compare]

  type view =
    | Overview
    | Customer
  [@@deriving compare]

  type t =
    { customers : customer Int.Map.t option
    ; customer_table : Customer_table.Model.t
    ; customer_form : Customer_form.Model.t
    ; errors : Errors.Model.t
    ; view : view
    ; last_search : Remote.Customers.filter option
    ; search : Form.State.t
    ; page : int
    ; token : Remote.Auth.token }
  [@@deriving compare, fields]

  let cutoff t1 t2 = compare t1 t2 = 0
end

let search_form = Form.(create ~name:"keyword search" Description.string)

let init () : Model.t =
  { Model.customers = None
  ; customer_table = Customer_table.Model.create ()
  ; customer_form = Customer_form.Model.create ()
  ; errors = Errors.Model.empty
  ; view = Model.Overview
  ; last_search = None
  ; page = 0
  ; search = Form.State.create ~init:"" search_form
  ; token = Remote.Auth.invalid_token }
;;

module Action = struct
  type t =
    | NavChange of Nav.t option sexp_opaque
    | Search
    | ResetSearch
    | GetMore
    | CustomerTable of Customer_table.Action.t
    | CustomerForm of Customer_form.Action.t
    | Errors of Errors.Action.t
    | GotCustomers of int (* page *) * (int * Customer.t) list Or_error.t
    | GotToken of Remote.Auth.token Or_error.t
  [@@deriving sexp_of, variants]
end

let view_head inject last_search state =
  let open Vdom in
  let fld_id = Form.State.field_ids state search_form in
  Node.create
    "form"
    [Attr.on "submit" (fun _ -> inject Action.Search)]
    [ Bs.Grid.(
        frow
          ~c:["mb-4"; "mt-2"]
          [ col_auto
              [ div
                  [A.class_ "input-group"]
                  [ div
                      [A.class_ "input-group-prepend"]
                      [ Bs.button
                          ~i:(S "undo")
                          ~action:(fun _ -> inject Action.ResetSearch)
                          "Zurücksetzen" ]
                  ; Form.Input.text
                      state
                      fld_id
                      ( [Attr.class_ "form-control"; Attr.placeholder "Schlüsselwort"]
                      @
                      match last_search with
                      | None -> []
                      | Some (Remote.Customers.Keyword s) -> [Attr.value s] )
                  ; div [A.class_ "input-group-append"] [Bs.button_submit "Suchen"] ] ]
          ; col
              [ frow
                  ~c:["justify-content-end"]
                  [Bs.button' ~href:Nav.(href_of (Customer New)) "Neuer Kunde"] ] ]) ]
;;

let get_token ~schedule_action =
  Request.XHR.send'
    Remote.Auth.get_token
    ~handler:(Fn.compose schedule_action Action.gottoken)
;;

let customer_page_size = 250

let get_customers ~token ~schedule_action ?(page = 0) ?filter () =
  let offset = if page > 0 then Some ((page * customer_page_size) + 1) else None in
  Request.XHR.send'
    Remote.Customers.(get ?offset ~limit:customer_page_size ?filter token)
    ~handler:(fun r -> schedule_action (Action.GotCustomers (page, r)))
;;

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let customers = model >>| Model.customers >>| Option.value ~default:Int.Map.empty in
  let table =
    let model = model >>| Model.customer_table
    and old_model = old_model >>| Model.customer_table
    and inject = Fn.compose inject Action.customertable
    and rows =
      Incr_map.mapi customers ~f:(fun ~key:_ ~data ->
          Customer_table.Model.Row.of_customer ~id:data.id data.data )
    in
    Customer_table.create rows ~old_model ~inject ~model
  in
  let customer =
    let inject = Fn.compose inject Action.customerform
    and back_href = Nav.(href_of Overview)
    and form_model = model >>| Model.customer_form
    and token = model >>| Model.token in
    Customer_form.create ~inject ~back_href token form_model
  in
  let errors =
    let inject = Fn.compose inject Action.errors
    and model = model >>| Model.errors in
    Errors.create ~inject model
  in
  let%map table = table
  and model = model
  and customer = customer
  and errors = errors
  and search_state = model >>| Model.search
  and last_search = model >>| Model.last_search in
  let apply_action (a : Action.t) state ~schedule_action =
    match a with
    | GotCustomers (_, Error detail) ->
      State.log_error state {gist = "Verbindungsfehler (Kunden)"; detail};
      model
    | GotCustomers (page, Ok l) ->
      let customers =
        List.mapi l ~f:(fun i (id, data) ->
            i + (page * customer_page_size), Model.{id; data} )
        |> Int.Map.of_alist_or_error
        |> function
        | Error detail ->
          State.log_error state {gist = "Laden von Kunden fehlgeschlagen"; detail};
          model.customers
        | Ok m ->
          if page > 0
          then (
            let old = Option.value ~default:Int.Map.empty model.customers in
            match Int.Map.append ~lower_part:old ~upper_part:m with
            | `Ok m -> Some m
            | `Overlapping_key_ranges ->
              State.log_error
                state
                { gist = "Laden von Kunden fehlgeschlagen"
                ; detail = Error.of_string "Overlapping key ranges" };
              model.customers )
          else Some m
      in
      {model with customers; page}
    | GotToken (Ok token) ->
      (* schedule renewal, token is valid for 300s. *)
      Async_kernel.upon (Async_js.sleep 240.) (fun () -> get_token ~schedule_action);
      {model with token}
    | GotToken (Error detail) ->
      (* schedule retry *)
      Async_kernel.upon (Async_js.sleep 1.) (fun () -> get_token ~schedule_action);
      State.log_error state {gist = "Verbindungsfehler (Token)"; detail};
      (* TODO: retry and showing error might confuse user *)
      model
    | CustomerTable a ->
      let schedule_action = Fn.compose schedule_action Action.customertable in
      let customer_table = Component.apply_action ~schedule_action table a () in
      {model with customer_table}
    | CustomerForm a ->
      let schedule_action = Fn.compose schedule_action Action.customerform in
      let customer_form = Component.apply_action ~schedule_action customer a state in
      {model with customer_form}
    | Errors a ->
      let schedule_action = Fn.compose schedule_action Action.errors in
      let errors = Component.apply_action ~schedule_action errors a state in
      {model with errors}
    | NavChange None ->
      Nav.(set Overview);
      model
    | NavChange (Some (Customer x)) ->
      schedule_action (Action.CustomerForm (Customer_form.Action.navchange x));
      {model with view = Customer}
    | NavChange (Some Overview) ->
      let token = model.token
      and filter = model.last_search in
      get_customers ?filter ~token ~schedule_action ();
      {model with view = Overview}
    | Search ->
      let pattern_o, search = Form.State.read_value model.search search_form in
      (match pattern_o with
      | None -> model
      | Some s ->
        let token = model.token
        and filter =
          let open String in
          let s = strip s in
          let l, r = is_prefix ~prefix:"_" s, is_suffix ~suffix:"_" s in
          match strip ~drop:(function '_' -> true | _ -> false) s with
          | "" -> None
          | s ->
            let s = (if l then "" else "%") ^ s ^ if r then "" else "%" in
            Some (Remote.Customers.Keyword s)
        in
        get_customers ~token ~schedule_action ?filter ();
        {model with search; last_search = filter})
    | GetMore ->
      let page = model.page + 1
      and token = model.token
      and filter = model.last_search in
      get_customers ~page ~token ~schedule_action ?filter ();
      model
    | ResetSearch ->
      let token = model.token in
      get_customers ~token ~schedule_action ();
      {model with last_search = None; search = Form.State.create search_form}
  and view =
    let open Vdom in
    let attr, tl =
      match model.view with
      | Overview ->
        ( [Attr.on "scroll" (fun _ -> Event.Viewport_changed)]
        , [ view_head inject last_search search_state
          ; (if Option.is_some model.customers
            then Component.view table
            else Bs.Grid.loading_row) ]
          @
          (match model.customers with
          | Some m when Int.Map.length m = (model.page + 1) * customer_page_size ->
            let open Bs.Grid in
            [ row
                ~c:["justify-content-center"]
                [ col_auto
                    ~c:["mb-2"]
                    [Bs.button ~action:(fun _ -> inject Action.GetMore) "Mehr"] ] ]
          | _ -> []) )
      | Customer -> [], [Component.view customer]
    and top =
      let open Bs.Grid in
      row
        ~c:["justify-content-end"; "headline"]
        [ col_auto
            Node.
              [ text "Angemeldet als "
              ; create "b" [] [text (Remote.Auth.username model.token)]
              ; text ". "
              ; a [Attr.href "logout.php"] [text "Abmelden."] ] ]
    in
    Node.div attr (Component.view errors :: top :: tl)
  and update_visibility ~schedule_action : Model.t =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    let customer_table = Component.update_visibility table ~schedule_action in
    {model with customer_table}
  and on_display _state ~schedule_action =
    let schedule_action = Fn.compose schedule_action Action.customertable in
    Component.on_display table ~schedule_action ()
  in
  Component.create ~update_visibility ~apply_action ~on_display model view
;;

let on_startup ~schedule_action _model =
  (* Wait for jwt token before doing anything else *)
  let open Async_kernel in
  Request.XHR.Deferred.send' Remote.Auth.get_token
  >>| fun response ->
  schedule_action (Action.GotToken response);
  Nav.listen (Fn.compose schedule_action Action.navchange);
  schedule_action (Action.NavChange (Nav.get ()));
  let handle_error e = schedule_action (Action.Errors (Errors.Action.log e)) in
  State.create ~handle_error
;;
