open! Base
module E = Lwt_react.E
module S = Lwt_react.S

let hooks = Stdio.In_channel.stdin

let run_hc (cmd : string) =
  Stdio.In_channel.input_all (Unix.open_process_in cmd)

let counts () =
  {cmd|herbstclient foreach --unique T tags sprintf STR "%{%c.client_count}" T echo STR|cmd}
  |> run_hc |> String.strip |> String.split ~on:'\n'
  |> List.map ~f:Int.of_string |> Array.of_list

let current_tag () =
  {cmd|herbstclient substitute T tags.focus.index echo T|cmd} |> run_hc
  |> String.strip |> Int.of_string

let reload () = {cmd|herbstclient reload|cmd} |> run_hc |> ignore

let watch_tags () =
  {cmd|herbstclient foreach --unique T tags sprintf STR "%c.client_count" T watch STR|cmd}
  |> run_hc |> ignore

type hook =
  | Tag_added of { tag_name : string }
  | Tag_changed of { tag_name : string; monitor : int }
  | Focus_changed of { winid : int; title : string }
  | Attribute_changed of {
      path : string list;
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
  | [ "tag_added"; tag_name ] -> Tag_added { tag_name }
  | [ "focus_changed"; winid; title ] ->
      Focus_changed { winid = ofs winid; title }
  | [ "attribute_changed"; path; old_value; new_value ] ->
      Attribute_changed
        { path = String.split ~on:'.' path; new_value; old_value }
  | _ -> Unknown hook

module ReactBar = struct
  module Tags = struct
    type t = {
      size : int;
      names : ((string, int, String.comparator_witness) Map.t[@sexp.opaque]);
      _names : string Array.t;
      counts : int Array.t;
    }
    [@@deriving sexp_of]

    let indices
        (names : ((string, int, String.comparator_witness) Map.t[@sexp.opaque]))
        =
      let indices = Array.create ~len:(Map.length names) "" in
      let iter (name, i) = Array.set indices i name in
      List.iter ~f:iter (Map.to_alist names);
      indices

    let add_tag (t : t) (name : string) : t =
      let names = Map.add_exn t.names ~key:name ~data:t.size in
      let counts = counts () in
      { size = t.size + 1; names; _names = indices names; counts }

    let set_count (t : t) ~(tag_index : int) ~(count : int) =
      Array.set t.counts tag_index count;
      t

    let create () =
      let names = Map.empty (module String) in
      let names = Map.add_exn names ~key:"default" ~data:0 in
      let _names = [| "default" |] in
      let counts = counts () in
      { size = 1; names; counts; _names }
  end

  module Status = struct
    type t = { bytes : Bytes.t; loc : (int * int) Array.t }

    let size (name : string) =
      4 + (2 * String.length "^fg(#xxxxxx)") + String.length name

    let write_string ~(bytes : Bytes.t) ~(offset : int) (s : string) =
      let set = Bytes.set bytes in
      String.iteri s ~f:(fun i c -> set (offset + i) c);
      String.length s

    let draw_tag ~(name : string) ~(offset : int) ~(bytes : Bytes.t) : int =
      let set offset c =
        Bytes.set bytes offset c;
        1
      in
      let offset = offset + write_string ~bytes ~offset "^fg(#FFFFFF)" in
      let offset = offset + write_string ~bytes ~offset "^fg(#000000)" in
      let offset = offset + set offset '[' in
      let offset = offset + set offset ' ' in
      let offset = offset + write_string ~bytes ~offset name in
      let offset = offset + set offset ' ' in
      let _ = offset + set offset ']' in
      size name

    let set_focus (t : t) ~(tag_index : int) : unit =
      let offset, _ = Array.get t.loc tag_index in
      let bytes = t.bytes in
      let offset = offset + write_string ~bytes ~offset "^fg(#000000)" in
      let _ = write_string ~bytes ~offset "^bg(#FFDD33)" in
      ()

    let set_unfocus (t : t) ~(tag_index : int) : unit =
      let offset, _ = Array.get t.loc tag_index in
      let bytes = t.bytes in
      let offset = offset + write_string ~bytes ~offset "^fg(#FFFFFF)" in
      let _ = write_string ~bytes ~offset "^bg(#000000)" in
      ()

    let set_empty (t : t) ~(tag_index : int) : unit =
      let offset, _ = Array.get t.loc tag_index in
      let bytes = t.bytes in
      let offset = offset + write_string ~bytes ~offset "^fg(#888888)" in
      let _ = write_string ~bytes ~offset "^bg(#000000)" in
      ()

    let create (tags : Tags.t) : t =
      let focused = current_tag () in
      let size_names =
        List.fold ~init:0 ~f:(fun s n -> s + size n) (Map.keys tags.names)
      in
      let size_names = size_names + String.length "^fg(#000000)" in
      let bytes = Bytes.make size_names '_' in
      let loc = Array.create ~len:tags.size (0, 0) in
      let t = { bytes; loc } in
      let fold tag_index offset name =
        let size = draw_tag ~name ~offset ~bytes in
        Array.set loc tag_index (offset, size);
        (if Array.get tags.counts tag_index > 0 then set_unfocus else set_empty)
          t ~tag_index;
        if tag_index = focused then set_focus t ~tag_index;
        offset + size
      in
      let offset = List.foldi (Array.to_list tags._names) ~f:fold ~init:0 in
      let _ = write_string ~bytes ~offset "^fg(#000000)" in
      t
  end

  type t = { tags : Tags.t; tag_focused : int; status : Status.t }

  let apply (t : t) (event : hook) =
    match event with
    | Tag_added { tag_name } ->
        let tags = Tags.add_tag t.tags tag_name in
        { t with tags; status = Status.create tags }
    | Attribute_changed
        { path = [ "tags"; tag_index; "client_count" ]; new_value; _ } ->
        let tag_index = Int.of_string tag_index in
        let count = Int.of_string new_value in
        { t with tags = Tags.set_count t.tags ~tag_index ~count }
    | Tag_changed { tag_name; _ } ->
        let tag_focused = Map.find_exn t.tags.names tag_name in
        let () = Status.set_focus t.status ~tag_index:tag_focused in
        let () =
          (if Array.get t.tags.counts t.tag_focused > 0 then Status.set_unfocus
          else Status.set_empty)
            t.status ~tag_index:t.tag_focused
        in
        { t with tag_focused }
    | _ -> t

  let create' () =
    let tags = Tags.create () in
    let tag_focused = current_tag () in
    { tags; tag_focused; status = Status.create tags }

  let create event =
    let t =
      let tags = Tags.create () in
      { tags; tag_focused = 0; status = Status.create tags }
    in
    let t = S.fold ~eq:(fun _ _ -> false) apply t event in
    t
end

let run () =
  let bar = ReactBar.create' () in
  let () = watch_tags () in
  let () = reload () in
  let () = Out_channel.set_binary_mode Stdio.stdout true in
  let rec loop (bar : ReactBar.t) =
    let line = Stdio.In_channel.input_line_exn Stdio.stdin in
    let hook = parse_hook line in
    let bar = ReactBar.apply bar hook in
    let () =
      Stdio.Out_channel.output_string Stdio.stdout
        (Bytes.to_string bar.status.bytes)
    in
    let () = Stdio.Out_channel.output_char Stdio.stdout '\n' in
    let () = Stdio.Out_channel.flush Stdio.stdout in
    loop bar
  in
  loop bar
