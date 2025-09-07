open! Base

let hooks = Stdio.In_channel.stdin

let run_hc (cmd : string) =
  Stdio.In_channel.input_all (Unix.open_process_in cmd)

module B = struct
  type t = {
    n_tags : int;
    status : Bytes.t;
    current_tag : int ref;
    tag_names : (string, int) Hashtbl.t;
    counts : int Array.t;
  }

  let watch_tags () =
    {cmd|herbstclient foreach --unique --filter-name="[0-9]" T tags sprintf STR "%c.client_count" T watch STR|cmd}
    |> run_hc |> ignore

  let current_tag () =
    {cmd|herbstclient substitute T tags.focus.index echo T|cmd} |> run_hc
    |> String.strip |> Int.of_string

  let counts () =
    {cmd|herbstclient foreach --unique T tags.by-name sprintf STR "%{%c.client_count}" T echo STR|cmd}
    |> run_hc |> String.strip |> String.split ~on:'\n'
    |> List.map ~f:Int.of_string |> Array.of_list

  let tag_names () =
    let f s =
      let l = String.split s ~on:' ' in
      let name = List.nth_exn l 0 in
      let index = List.nth_exn l 1 in
      (name, Int.of_string index)
    in
    {cmd|herbstclient foreach --unique --filter-name="[0-9]" T tags sprintf STR "%{%c.name} %{%c.index}" T T echo STR|cmd}
    |> run_hc |> String.strip |> String.split ~on:'\n' |> List.map ~f
    |> Hashtbl.of_alist_exn (module String)

  let set_status bar tag_index c = Bytes.set bar.status ((3 * tag_index) + 1) c
  let set_empty bar tag_index = set_status bar tag_index '_'
  let set_nonempty bar tag_index = set_status bar tag_index '*'
  let set_focused bar tag_index = set_status bar tag_index 'x'
  let set_count bar ~tag_index ~count = Array.set bar.counts tag_index count
  let get_count bar ~tag_index = Array.get bar.counts tag_index

  let tag_index bar ~(tag_name : string) : int =
    Hashtbl.find_exn bar.tag_names tag_name

  let create (n_tags : int) =
    let bar =
      {
        n_tags;
        status = Bytes.make (3 * n_tags) '[';
        counts = counts ();
        tag_names = tag_names ();
        current_tag = ref (current_tag ());
      }
    in
    let () =
      let f tag_index c =
        if c > 0 then set_nonempty bar tag_index else set_empty bar tag_index;
        Bytes.set bar.status ((3 * tag_index) + 2) ']'
      in
      Array.iteri ~f bar.counts
    in
    set_focused bar !(bar.current_tag);
    bar

  let render (bar : t) =
    let open Stdio.Out_channel in
    output_bytes stdout bar.status;
    output_char stdout '\n';
    flush stdout
end

let bar = B.create 9

type hook =
  | Tag_changed of { tag_name : string; monitor : int }
  | Focus_changed of { winid : int; title : string }
  | Attribute_changed of {
      path : string;
      new_value : string;
      old_value : string;
    }
  | Unknown of string
[@@deriving sexp]

let parse_hook (hook : string) : hook =
  let ofs = Int.of_string in
  let s = String.split ~on:'\t' hook in
  match s with
  | [ "tag_changed"; tag_name; monitor ] ->
      Tag_changed { tag_name; monitor = ofs monitor }
  | [ "focus_changed"; winid; title ] ->
      Focus_changed { winid = ofs winid; title }
  | [ "attribute_changed"; path; old_value; new_value ] ->
      Attribute_changed { path; new_value; old_value }
  | _ -> Unknown hook

let process_attribute_changed ~path ~new_value ~old_value =
  let () = ignore old_value in
  match String.split ~on:'.' path with
  | "tags" :: "by-name" :: _ -> ()
  | [ "tags"; tag_index; "client_count" ] -> (
      let tag_index = Int.of_string tag_index in
      let count = Int.of_string new_value in
      let () = B.set_count bar ~tag_index ~count in
      match (tag_index = !(bar.current_tag), count) with
      | false, 0 -> B.set_empty bar tag_index
      | false, _ -> B.set_nonempty bar tag_index
      | _ -> ())
  | _ -> ()

let process_hook hook =
  let hook = parse_hook hook in
  (* let () = Stdio.print_s (sexp_of_hook hook) in *)
  let () =
    match hook with
    | Tag_changed { tag_name; _ } ->
        let tag_index = B.tag_index bar ~tag_name in
        if B.get_count bar ~tag_index:!(bar.current_tag) > 0 then
          B.set_nonempty bar !(bar.current_tag)
        else B.set_empty bar !(bar.current_tag);
        bar.current_tag := tag_index;
        B.set_focused bar tag_index
    | Focus_changed _ -> ()
    | Attribute_changed { path; new_value; old_value } ->
        process_attribute_changed ~path ~new_value ~old_value
    | Unknown _ -> ()
  in
  B.render bar

let () = B.render bar
let () = B.watch_tags ()

let rec process_hooks () =
  let hook = Stdio.In_channel.input_line hooks in
  let () = ignore (Option.map ~f:process_hook hook) in
  process_hooks ()
