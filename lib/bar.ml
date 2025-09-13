open! Base
module E = Lwt_react.E

module Color = struct
  type t = string
end

module type Colors = sig
  val size : int
  val fg_color : Color.t -> string
  val bg_color : Color.t -> string
end

module Dzen2 : Colors = struct
  let size = 4 + 7 + 1
  let fg_color (color : Color.t) = "^fg(" ^ color ^ ")"
  let bg_color (color : Color.t) = "^bg(" ^ color ^ ")"
end

module type Config_sig = sig
  val size : int
  val fg : Color.t
  val focused_fg : Color.t
  val empty_fg : Color.t
  val nonempty_fg : Color.t
  val focused_bg : Color.t
  val empty_bg : Color.t
  val nonempty_bg : Color.t
end

module Config (C : Colors) : Config_sig = struct
  let size = C.size
  let fg = C.fg_color "#000000"
  let focused_fg = C.fg_color "#000000"
  let focused_bg = C.bg_color "#FFDD33"
  let empty_fg = C.fg_color "#888888"
  let empty_bg = C.bg_color "#000000"
  let nonempty_fg = C.fg_color "#FFFFFF"
  let nonempty_bg = C.bg_color "#000000"
end

module Tags = struct
  type t = {
    size : int;
    names : ((string, int, String.comparator_witness) Map.t[@sexp.opaque]);
    _names : string Array.t;
    counts : int Array.t;
  }
  [@@deriving sexp_of]

  let indices
      (names : ((string, int, String.comparator_witness) Map.t[@sexp.opaque])) =
    let indices = Array.create ~len:(Map.length names) "" in
    let iter (name, i) = Array.set indices i name in
    List.iter ~f:iter (Map.to_alist names);
    indices

  let add_tag (t : t) (name : string) : t =
    let names = Map.add_exn t.names ~key:name ~data:t.size in
    let counts = Herbstclient.counts () in
    { size = t.size + 1; names; _names = indices names; counts }

  let set_count (t : t) ~(tag_index : int) ~(count : int) =
    Array.set t.counts tag_index count;
    t

  let create () =
    let names = Map.empty (module String) in
    let names = Map.add_exn names ~key:"default" ~data:0 in
    let _names = [| "default" |] in
    let counts = Herbstclient.counts () in
    { size = 1; names; counts; _names }
end

module type Status_sig = sig
  type t

  val bytes : t -> Bytes.t
  val set_focus : t -> tag_index:int -> unit
  val set_empty : t -> tag_index:int -> unit
  val set_nonempty : t -> tag_index:int -> unit
  val create : Tags.t -> int -> t
end

module Status (C : Config_sig) : Status_sig = struct
  type t = { bytes : Bytes.t; loc : (int * int) Array.t }

  let bytes (t : t) = t.bytes
  let size (name : string) = 4 + (2 * C.size) + String.length name

  let write_string ~(bytes : Bytes.t) ~(offset : int) (s : string) =
    let set = Bytes.set bytes in
    String.iteri s ~f:(fun i c -> set (offset + i) c);
    String.length s

  let draw_tag ~(name : string) ~(offset : int) ~(bytes : Bytes.t) : int =
    let set offset c =
      Bytes.set bytes offset c;
      1
    in
    let offset = offset + write_string ~bytes ~offset C.empty_fg in
    let offset = offset + write_string ~bytes ~offset C.empty_fg in
    let offset = offset + set offset '(' in
    let offset = offset + set offset ' ' in
    let offset = offset + write_string ~bytes ~offset name in
    let offset = offset + set offset ' ' in
    let _ = offset + set offset ')' in
    size name

  let set_focus (t : t) ~(tag_index : int) : unit =
    let offset, _ = Array.get t.loc tag_index in
    let bytes = t.bytes in
    let offset = offset + write_string ~bytes ~offset C.focused_fg in
    let _ = write_string ~bytes ~offset C.focused_bg in
    ()

  let set_nonempty (t : t) ~(tag_index : int) : unit =
    let offset, _ = Array.get t.loc tag_index in
    let bytes = t.bytes in
    let offset = offset + write_string ~bytes ~offset C.nonempty_fg in
    let _ = write_string ~bytes ~offset C.nonempty_bg in
    ()

  let set_empty (t : t) ~(tag_index : int) : unit =
    let offset, _ = Array.get t.loc tag_index in
    let bytes = t.bytes in
    let offset = offset + write_string ~bytes ~offset C.empty_fg in
    let _ = write_string ~bytes ~offset C.empty_bg in
    ()

  let create (tags : Tags.t) (current_tag : int) : t =
    let focused = current_tag in
    let size_names =
      List.fold ~init:0 ~f:(fun s n -> s + size n) (Map.keys tags.names)
    in
    let size_names = size_names + C.size in
    let bytes = Bytes.make size_names '_' in
    let loc = Array.create ~len:tags.size (0, 0) in
    let t = { bytes; loc } in
    let fold tag_index offset name =
      let size = draw_tag ~name ~offset ~bytes in
      Array.set loc tag_index (offset, size);
      (if Array.get tags.counts tag_index > 0 then set_nonempty else set_empty)
        t ~tag_index;
      if tag_index = focused then set_focus t ~tag_index;
      offset + size
    in
    let offset = List.foldi (Array.to_list tags._names) ~f:fold ~init:0 in
    let _ = write_string ~bytes ~offset C.fg in
    t
end

module C = Config (Dzen2)
module S = Status (C)

type t = { tags : Tags.t; tag_focused : int; status : S.t }

let apply (t : t) (event : Hook.t) =
  match event with
  | Tag_added { tag_name } ->
      let tags = Tags.add_tag t.tags tag_name in
      { t with tags; status = S.create tags t.tag_focused }
  | Attribute_changed
      { path = [ "tags"; tag_index; "client_count" ]; new_value; _ } ->
      let tag_index = Int.of_string tag_index in
      let count = Int.of_string new_value in
      { t with tags = Tags.set_count t.tags ~tag_index ~count }
  | Tag_changed { tag_name; _ } ->
      let tag_focused = Map.find_exn t.tags.names tag_name in
      let () = S.set_focus t.status ~tag_index:tag_focused in
      let () =
        (if Array.get t.tags.counts t.tag_focused > 0 then S.set_nonempty
        else S.set_empty)
          t.status ~tag_index:t.tag_focused
      in
      { t with tag_focused }
  | _ -> t

let create (current_tag : int) =
  let tags = Tags.create () in
  let tag_focused = current_tag in
  { tags; tag_focused; status = S.create tags tag_focused }

let hooks = Stdio.In_channel.stdin

let run (current_tag : int) =
  let bar = create current_tag in
  let () = Herbstclient.watch_tags () in
  let () = Herbstclient.reload () in
  let () = Out_channel.set_binary_mode Stdio.stdout true in
  let rec loop (bar : t) =
    let line = Stdio.In_channel.input_line_exn Stdio.stdin in
    let hook = Hook.parse_hook line in
    let bar = apply bar hook in
    let () = Stdio.Out_channel.output_bytes Stdio.stdout (S.bytes bar.status) in
    let () = Stdio.Out_channel.output_char Stdio.stdout '\n' in
    let () = Stdio.Out_channel.flush Stdio.stdout in
    loop bar
  in
  loop bar
