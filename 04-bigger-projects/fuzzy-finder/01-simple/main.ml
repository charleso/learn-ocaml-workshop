open! Core
open! Async

let run filter =
  let
    re = Re.compile (Re.str filter)
  in
    Pipe.iter (Reader.lines (force Reader.stdin)) ~f:(fun line ->
        (* EXERCISE: Filter down to the matching lines.

           You can use Re for the pattern matching. Check out the functions:

           - Re.str
           - Re.compile
           - Re.execp
        *)

        if Re.execp ~pos:0 ~len:(-1) re line then
          print_endline line
        else
          ();
        (* Manually flush inline *)
        (* Writer.flushed (force Writer.stdout) *)
        Deferred.unit)

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Read stdin, spit out lines that contain a match of the string in question"
    (let%map_open filter = anon ("filter" %: string) in
     fun () -> run filter)

let () = Command.run command
