open! Base
open Import
module Widget = Tty_text.Widget

module Model = struct
  type t =
    { lines: string Map.M(Int).t
    ; start: Time.t
    ; filter: string
    ; selected: int
    ; dim: Tty_text.Dimensions.t
    }

  let create ~now =
    { lines = Map.empty (module Int)
    ; start = now
    ; filter = ""
    ; selected = 0
    ; dim = { width = 0; height = 0 }
    }

  let widget_and_selected t ~now =
    let elapsed = Widget.of_string (Time.Span.to_string (Time.diff now t.start)) in
    let line = Widget.of_string (Int.to_string (Map.length t.lines)) in
    let re = Re.compile (Re.str t.filter) in
    (* let () = Stdio.print_endline (Int.to_string t.dim.height) in *)
    let highlight rest = "\x1b[32m" ^ rest ^ "\x1b[0m" in
    let flines =
         (Map.range_to_alist ~min:0 ~max:(Map.length t.lines) t.lines) |> List.map ~f:snd
      |> List.filter ~f:(Re.execp re)
      in
    let selected = List.nth flines t.selected in
    let wlines =
         List.take flines (t.dim.height - 2)
      |> List.map ~f:(String.substr_replace_all ~pattern:t.filter ~with_:(highlight t.filter))
      |> List.mapi ~f:(fun i x -> if i = t.selected then "> " ^ x else "  " ^ x)
      |> fun l ->
        List.append l
          (List.init (t.dim.height - 2 - List.length l) ~f:(Fn.const ""))
      |> List.rev
      |> List.map ~f:Widget.of_string
      |> Widget.vbox
      in
    let widget = Widget.hbox [ elapsed; line ] in
    let wfilter = Widget.of_string ("> " ^ t.filter) in
    (Widget.vbox [wlines; widget; wfilter], selected)
end

module Action = struct
  type t =
    | Exit
    | Exit_and_print
end

let handle_user_input (m:Model.t) (input:Tty_text.User_input.t) =
  match input with
  | Ctrl_c -> (m, Some Action.Exit)
  | Char c -> ({ m with filter = m.filter ^ Char.to_string c }, None)
  | Backspace -> ({ m with filter = String.drop_suffix m.filter 1 }, None)
  | Return -> (m, Some Action.Exit_and_print)
  | Up -> ({ m with selected = m.selected + 1}, None)
  | Down -> ({ m with selected = max 0 (m.selected - 1) }, None)
  | _ -> (m, None)

let handle_line (m:Model.t) line : Model.t =
  let lnum =
    match Map.max_elt m.lines with
    | None -> 1
    | Some (num,_) -> num + 1
  in
  { m with lines = Map.set m.lines ~key:lnum ~data:line }

let handle_closed (m:Model.t) _ =
  m

let set_dim (m:Model.t) dim =
  { m with dim = dim }
