open Core
open Import
open Async

let run user_input tty_text =
  let (!) = Incr.Var.value in
  let (:=) = Incr.Var.set in
  let now = Time.now () in
  let stdin = force Reader.stdin in
  let model_v = Incr.Var.create (Fuzzy.Model.create ~now) in
  let widget_and_selected = Fuzzy.Model.widget_and_selected (Incr.Var.watch model_v) in
  let finished = Ivar.create () in
  don't_wait_for (
    Pipe.iter_without_pushback (Reader.lines stdin) ~f:(fun line ->
        model_v := Fuzzy.handle_line !model_v line));
  upon (Reader.close_finished stdin) (fun () ->
      model_v := Fuzzy.handle_closed !model_v (Incr.now ()));
  let widget_to_render = ref None in
  let last_selected = ref None in
  Incr.Observer.on_update_exn (Incr.observe widget_and_selected)
    ~f:(function
        | Initialized (widget,selected) | Changed (_, (widget,selected)) ->
          Ref.(widget_to_render := Some widget; last_selected := selected)
        | Invalidated -> assert false);
  don't_wait_for (
    Pipe.iter_without_pushback user_input ~f:(fun input ->
        let (model,action) = Fuzzy.handle_user_input !model_v input in
        model_v := model;
        match action with
        | None -> ()
        | Some Exit ->
          Ivar.fill finished None
        | Some Exit_and_print ->
          Ivar.fill finished Ref.(!last_selected)));
  let finished = Ivar.read finished in
  Clock.every' (sec 0.1) ~stop:(Deferred.ignore finished)
    (fun () ->
       let dim = Tty_text.dimensions tty_text in
       model_v := Fuzzy.set_dim !model_v dim;
       Incr.advance_clock ~to_:(Time.now ());
       Incr.stabilize ();
       match Ref.(!widget_to_render) with
       | None -> Deferred.unit
       | Some widget ->
         Ref.(widget_to_render := None);
         Tty_text.render tty_text widget);
  finished

let command =
  let open Command.Let_syntax in
  Command.async ~summary:"Simplified clone of fzf"
    (let%map_open () = return () in
     fun () ->
       Log.Global.set_output [Log.Output.file `Sexp ~filename:".fuzzy.log"];
       Log.Global.sexp [%message "Starting up"];
       let open Deferred.Let_syntax in
       match%bind
         try_with (fun () ->
             Tty_text.with_rendering (fun (input, tty_text) ->
                 run input tty_text))
       with
       | Error err ->
         print_s [%message "Failed with an exception" (err : exn)];
         Writer.flushed (force Writer.stdout)
       | Ok None ->
         return ()
       | Ok (Some output) ->
         print_endline output;
         Writer.flushed (force Writer.stdout))
