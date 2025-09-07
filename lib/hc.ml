open! Base

module Root = struct
  type 'a attr = String : string -> string attr

  module Clients = struct
    let get (attr : string) = String attr

    module Focus = struct
      let class_ = String "class_"
    end
  end
end

module Field = struct
  type obj
  type 'a attr

  type _ t =
    | Obj : string -> obj t
    | SubO : obj t * string -> obj t
    | Attr : obj t * string -> 'a attr t

  let clients = Obj "clients"
  let clients_focus = SubO (clients, "focus")
  let clients_focus_class : string attr t = Attr (clients_focus, "class")
  let clients_focus_title : string attr t = Attr (clients_focus, "title")
  let clients_focus_winid : int attr t = Attr (clients_focus, "winid")
  let tags = Obj "tags"
  let tags_focus = SubO (tags, "focus")
  let tags_focus_index : int attr t = Attr (tags_focus, "index")
  let tags_count : int attr t = Attr (tags, "count")

  let rec to_string : type a. a t -> string =
   fun t ->
    match t with
    | Obj s -> s
    | Attr (obj, s) -> to_string obj ^ "." ^ s
    | SubO (obj, s) -> to_string obj ^ "." ^ s
end

module H = struct
  type (_, _) var =
    | Z : ('a * _, 'a) var
    | S : ('env, 'a) var -> ('b * 'env, 'a) var

  type _ value =
    | VInt : int -> int value
    | VString : string -> string value
    | VUnit : unit -> unit value

  type _ typ = TInt : int typ | TString : string typ | TField : 'a Field.t typ

  type _ env =
    | Nil : unit env
    | Cons : 'a typ * string * 'env env -> ('a typ * 'env) env

  type (_, _) term =
    | Field : 'a Field.t -> ('env, 'a Field.t typ) term
    | Int : int -> ('env, int typ) term
    | Help : ('env, 'a Field.t typ) term -> ('env, unit typ) term
    | Eof : ('env, string typ) term
    | String : string -> ('env, string typ) term
    | Jumpto : ('env, string typ) term -> ('env, unit typ) term
    | ConcatString :
        ('env, string typ) term * ('env, string typ) term
        -> ('env, string typ) term
    | Quit : ('env, unit typ) term
    | And :
        (string * ('env, unit typ) term * ('env, unit typ) term)
        -> ('env, unit typ) term
    | Reload : ('env, unit typ) term
    | Echo : ('env, string typ) term -> ('env, unit typ) term
    | Var : ('env, 'a) var -> ('env, 'a) term
    | FieldToString : ('env, 'a Field.t typ) term -> ('env, string typ) term
    | Substitute :
        string
        * ('env, 'a Field.attr Field.t typ) term
        * ('a Field.attr Field.t typ * 'env, 'b) term
        -> ('env, 'b) term
    | SPrintf :
        string * 'env fmt * (string typ * 'env, 'b) term
        -> ('env, 'b) term
    | Foreach :
        string
        * ('env, Field.obj Field.t typ) term
        * (Field.obj Field.t typ * 'env, 'b) term
        -> ('env, 'b) term
    | CompareEq :
        ('env, 'a Field.attr Field.t typ) term * ('env, 'b typ) term
        -> ('env, unit typ) term

  and _ fmt =
    | Eof : 'env fmt
    | FmtText : ('env, string typ) term * 'env fmt -> 'env fmt
    | FmtAttr : ('env, 'a Field.t typ) term * 'env fmt -> 'env fmt
    | FmtAttr' : ('env, 'a Field.t typ) term * string * 'env fmt -> 'env fmt
    | FmtAttr2 : 'env fmt * string * 'env fmt -> 'env fmt

  let rec lookup_name : type a b. a env -> (a, b) var -> string =
   fun env var ->
    match (env, var) with
    | Nil, _ -> raise (Not_found_s (Atom "Empty stack"))
    | Cons (_, name, _), Z -> name
    | Cons (_, _, env), S var -> lookup_name env var

  let rec eval : type a b. Buffer.t -> a env -> (a, b) term -> unit =
   fun buffer env term ->
    match term with
    | CompareEq (term1, term2) ->
        Buffer.add_string buffer "compare ";
        eval buffer env term1;
        Buffer.add_string buffer " = ";
        eval buffer env term2
    | Jumpto term ->
        Buffer.add_string buffer "jumpto ";
        eval buffer env term
    | And (sep, term1, term2) ->
        Buffer.add_string buffer ("and " ^ sep ^ " ");
        eval buffer env term1;
        Buffer.add_string buffer (" " ^ sep ^ " ");
        eval buffer env term2
    | Field attr -> Buffer.add_string buffer (Field.to_string attr)
    | Int n -> Buffer.add_string buffer (Int.to_string n)
    | Eof -> ()
    | Help field ->
        Buffer.add_string buffer "help ";
        eval buffer env field
    | ConcatString (term1, term2) ->
        eval buffer env term1;
        eval buffer env term2
    | String s -> Buffer.add_string buffer s
    | Quit -> Buffer.add_string buffer "quit"
    | Reload -> Buffer.add_string buffer "reload"
    | Echo term ->
        Buffer.add_string buffer "echo ";
        eval buffer env term
    | Var var ->
        Buffer.add_string buffer " ";
        Buffer.add_string buffer (lookup_name env var);
        Buffer.add_string buffer " "
    | FieldToString term -> eval buffer env term
    | SPrintf (name, fmt, term) ->
        Buffer.add_string buffer ("sprintf " ^ name ^ " ");
        let b1, b2 = Buffer.(create 50, create 50) in
        (* Buffer.add_string b1 "\""; *)
        Buffer.add_string b2 " ";
        eval_fmt b1 b2 env fmt;
        Buffer.add_string buffer (Buffer.contents b1);
        Buffer.add_string buffer (Buffer.contents b2);
        eval buffer (Cons (TString, name, env)) term
    | Substitute (name, attr, term) ->
        Buffer.add_string buffer ("substitute " ^ name ^ " ");
        eval buffer env attr;
        Buffer.add_string buffer " ";
        eval buffer (Cons (TField, name, env)) term
    | Foreach (name, obj, term) ->
        Buffer.add_string buffer ("foreach " ^ name ^ " ");
        eval buffer env obj;
        Buffer.add_string buffer " ";
        eval buffer (Cons (TField, name, env)) term

  and eval_fmt : type a. Buffer.t -> Buffer.t -> a env -> a fmt -> unit =
   fun b1 b2 env fmt ->
    match fmt with
    | Eof -> () (* Buffer.add_string b1 "\"" *)
    | FmtAttr2 (fmt1, field, fmt) ->
        Buffer.add_string b1 "%{";
        eval_fmt b1 b2 env fmt1;
        Buffer.add_string b1 ("." ^ field ^ "}");
        eval_fmt b1 b2 env fmt
    | FmtText (text, fmt) ->
        Buffer.add_string b1 "%c";
        Buffer.add_string b2 " ";
        eval b2 env text;
        Buffer.add_string b2 " ";
        eval_fmt b1 b2 env fmt
    | FmtAttr (attr, fmt) ->
        Buffer.add_string b1 "%s";
        eval b2 env attr;
        Buffer.add_string b2 " ";
        eval_fmt b1 b2 env fmt
    | FmtAttr' (attr, field, fmt) ->
        Buffer.add_string b1 "%{";
        eval b1 env attr;
        Buffer.add_string b1 ("." ^ field ^ "}");
        eval_fmt b1 b2 env fmt

  let echo s = Echo s
end

module Helpers = struct
  open H

  let echo s = Echo s
  let and_ sep term1 term2 = And (sep, term1, term2)
  let jumpto winid = Jumpto winid

  let echo_client_field (field : string) :
      (Field.obj Field.t typ * unit, 'b) term =
    let obj = FmtText (FieldToString (Var Z), Eof) in
    let fmt = FmtAttr2 (obj, field, Eof) in
    SPrintf ("S", fmt, echo (Var Z))

  let foreach_client (fn : (Field.obj Field.t typ * 'a, 'b) term) :
      ('a, 'b) term =
    let clients = Field (Field.Obj "clients") in
    Foreach ("C", clients, fn)
end

module Examples = struct
  open H

  module Help = struct
    let ex : (unit, unit typ) term = Help (Field Field.clients)
  end

  module SPrintf = struct
    (* sprintf X %s/%s tags.focus.index tags.count echo X *)
    let ex3 : (unit, unit typ) term =
      let f =
        FmtAttr
          ( Field Field.tags_focus_index,
            FmtText (String "/", FmtAttr (Field Field.tags_count, Eof)) )
      in
      SPrintf ("X", f, echo (Var Z))

    (* sprintf VALUE "%{%c.client_count}" tags.0 echo VALUE *)
    let ex4 : (unit, unit typ) term =
      let f = FmtAttr' (Field Field.(Attr (tags, "0")), "client_count", Eof) in
      SPrintf ("VALUE", f, echo (Var Z))

    (* substitute X tags.count sprintf Y "number=%c" X echo Y *)
    let ex5 : (unit, unit typ) term =
      let cmd =
        SPrintf
          ( "Y",
            FmtText (String "number=", FmtText (FieldToString (Var Z), Eof)),
            echo (Var Z) )
      in
      Substitute ("X", Field Field.tags_count, cmd)

    (* sprintf Y "number=%s" tags.count echo Y *)
    let ex6 : (unit, unit typ) term =
      SPrintf
        ( "Y",
          FmtText (String "number=", FmtAttr (Field Field.tags_count, Eof)),
          echo (Var Z) )
  end

  module Foreach = struct
    let ex : (unit, unit typ) term =
      let obj = Field.SubO (Field.tags, "by-name") in
      Foreach ("T", Field obj, echo (FieldToString (Var Z)))
  end
end
