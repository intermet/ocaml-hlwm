open! Base

let run (cmd : string) = Stdio.In_channel.input_all (Unix.open_process_in cmd)

let counts () =
  {cmd|herbstclient foreach --unique T tags sprintf STR "%{%c.client_count}" T echo STR|cmd}
  |> run |> String.strip |> String.split ~on:'\n' |> List.map ~f:Int.of_string
  |> Array.of_list

let current_tag () =
  {cmd|herbstclient substitute T tags.focus.index echo T|cmd} |> run
  |> String.strip |> Int.of_string

let reload () = {cmd|herbstclient reload|cmd} |> run |> ignore

let watch_tags () =
  {cmd|herbstclient foreach --unique T tags sprintf STR "%c.client_count" T watch STR|cmd}
  |> run |> ignore
