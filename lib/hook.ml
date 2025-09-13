open! Base

type t =
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

let parse_hook (hook : string) : t =
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
