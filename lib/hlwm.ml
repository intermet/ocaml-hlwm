open! Base

module Attribute = struct
  type winid = int

  type _ t =
    | Clients_focus_winid : winid t
    | Clients_focus_class : string t
    | Clients_focus_title : string t

  let to_string : type a. a t -> string = function
    | Clients_focus_class -> "clients.focus.class"
    | Clients_focus_winid -> "clients.focus.winid"
    | Clients_focus_title -> "clients.focus.title"
end

module Placeholder = struct
  type t = T1 | T2 | T3

  let to_string (t : t) = match t with T1 -> "T1" | T2 -> "T2" | T3 -> "T3"

  type (_, _) fmt =
    | Text : (string * ('a, 'b) fmt) -> (string, 'b) fmt
    | T : ('a, 'b) fmt -> ('a, t -> 'b) fmt
    | Eof : 'a Attribute.t -> ('a Attribute.t, string) fmt

  let rec ksprintf : type a b. Buffer.t -> (a, b) fmt -> b =
   fun b fmt ->
    match fmt with
    | Text (s, k) ->
        Buffer.add_string b s;
        ksprintf b k
    | T k ->
        fun t ->
          Buffer.add_string b (to_string t);
          ksprintf b k
    | Eof _ -> Buffer.contents b

  let eof attr = Eof attr
  let text s k = Text (s, k)
  let t k = T k
  let sprintf (fmt : _ fmt) = ksprintf (Buffer.create 64) fmt

  (* let p_fmt = sprintf arg *)
  (* let () = Stdio.Out_channel.print_endline p_fmt *)
end

module HlwmPrintf = struct
  type f = { format : Buffer.t; arg : Buffer.t }

  type _ fmt =
    | Text : (string * 'a fmt) -> 'a fmt
    | Const : 'a fmt -> (string -> 'a) fmt
    | AttrS : 'a fmt -> (_ Attribute.t -> 'a) fmt
    | Eof : (string * string) fmt

  let rec ksprintf : type a. f -> a fmt -> a =
   fun f fmt ->
    match fmt with
    | Text (s, k) ->
        Buffer.add_string f.format s;
        ksprintf f k
    | Const k ->
        fun v ->
          Buffer.add_string f.format "%c";
          Buffer.add_string f.arg v;
          ksprintf f k
    | AttrS k ->
        fun v ->
          Buffer.add_string f.format "%s";
          Buffer.add_string f.arg (Attribute.to_string v);
          ksprintf f k
        (* | Attr (p_fmt, k) -> *)
        (*     fun p -> *)
        (*       Buffer.add_string f.format "%{"; *)
        (*       Buffer.add_string f.arg (Placeholder.sprintf p_fmt p); *)
        (* ksprintf f k *)
    | Eof -> (Buffer.contents f.format, Buffer.contents f.arg)

  let sprintf fmt =
    ksprintf { format = Buffer.create 64; arg = Buffer.create 64 } fmt

  let text s k = Text (s, k)

  (* let attr k = AttrS k *)
  let const k = Const k
  let eof = Eof
  let myfmt = text "title=" (const eof)
  (* let s = sprintf myfmt Attribute.Clients_focus_title *)
  (* let () = Stdio.Out_channel.printf "format=%s, arg=%s\n" (fst s) (snd s) *)
end

let hc (cmd : string) = "herbstclient -n " ^ cmd

let run_hc (cmd : string) =
  let proc = Unix.open_process_in (hc cmd) in
  let output = Stdio.In_channel.input_all proc in
  let _ = Unix.close_process_in proc in
  output

type _ typ = Int : int typ | Unit : unit typ | String : string typ

type _ fn =
  | Value : 'a typ -> 'a fn
  | Function : string * 'a fn * 'b fn -> (string * ('a -> 'b)) fn

type _ obj = Tags : string obj

let string_of_obj : type a. a obj -> string = function Tags -> "tags"

(* type fullscreen = string *)

module M1 = struct
  type _ g = S : 'a g | Z : 'a g * 'b g -> ('a * 'b) g

  type _ term =
    | Int : int -> int term
    | String : string -> string term
    | Echo : string term -> unit term
    | Var : string -> int term
    | Succ : int term -> int term
    | Add : int term * int term -> int term
    | Let : string * int term * int term -> int term

  let one = Int 1
  let x = Var "X"
  let y = Add (x, one)
  let z = Add (one, one)
  let e = Let ("X", z, y)

  let rec eval : type a. (string, int) List.Assoc.t -> a term -> a =
   fun env term ->
    match term with
    | String s -> s
    | Echo (String s) -> Stdio.print_string s
    | Int n -> n
    | Var name -> List.Assoc.find_exn env ~equal:String.equal name
    | Succ term -> 1 + eval env term
    | Add (term1, term2) -> eval env term1 + eval env term2
    | Let (name, term1, term2) ->
        let env =
          List.Assoc.add env ~equal:String.equal name (eval env term1)
        in
        eval env term2
end

module M2 = struct
  type (_, _) var =
    | Z : ('a * _, 'a) var
    | S : ('env, 'a) var -> ('b * 'env, 'a) var

  type _ value =
    | VInt : int -> int value
    | VString : string -> string value
    | VUnit : unit -> unit value

  type _ env = Nil : unit env | Cons : 'a * 'env env -> ('a * 'env) env

  type (_, _) term =
    | Int : int -> ('env, int) term
    | String : string -> ('env, string) term
    | Echo : ('env, string) term -> ('env, unit) term
    | Var : ('env, 'a) var -> ('env, 'a) term
    | Succ : ('env, int) term -> ('env, int) term
    | Add : ('env, int) term * ('env, int) term -> ('env, int) term
    | Let : ('env, 'a) term * ('a * 'env, 'b) term -> ('env, 'b) term

  let one = Int 1
  let s = Var Z
  let f = Let (String "hi", Echo s)
  let a = Add (Var (S Z), Var Z)
  let e = Let (one, Let (one, a))

  let rec lookup : type a b. a env -> (a, b) var -> b =
   fun env var ->
    match (env, var) with
    | Nil, _ -> raise (Not_found_s (Atom "Empty stack"))
    | Cons (v, _), Z -> v
    | Cons (_, env), S var -> lookup env var

  let rec eval : type a b. a env -> (a, b) term -> b =
   fun env term ->
    match term with
    | Int n -> n
    | String s -> s
    | Echo term ->
        let t = eval env term in
        Stdio.print_string t
    | Succ n -> eval env n + 1
    | Add (t1, t2) -> eval env t1 + eval env t2
    | Var var -> lookup env var
    | Let (t1, t2) -> eval (Cons (eval env t1, env)) t2
end

module H = struct
  type (_, _) var =
    | Z : ('a * _, 'a) var
    | S : ('env, 'a) var -> ('b * 'env, 'a) var

  type _ value =
    | VInt : int -> int value
    | VString : string -> string value
    | VUnit : unit -> unit value

  type _ typ =
    | TInt : int typ
    | TString : string typ
    | TAttr : 'a Attribute.t typ

  type _ env =
    | Nil : unit env
    | Cons : 'a typ * string * 'env env -> ('a typ * 'env) env

  type (_, _) term =
    | Attribute : 'a Attribute.t -> ('env, 'a Attribute.t typ) term
    | Int : int -> ('env, int typ) term
    | Eof : ('env, string typ) term
    | String : string -> ('env, string typ) term
    | ConcatString :
        ('env, string typ) term * ('env, string typ) term
        -> ('env, string typ) term
    | Quit : ('env, unit typ) term
    | Reload : ('env, unit typ) term
    | Echo : ('env, string typ) term -> ('env, unit typ) term
    | Var : ('env, 'a) var -> ('env, 'a) term
    | AttributeToString :
        ('env, 'a Attribute.t typ) term
        -> ('env, string typ) term
    | Substitute :
        string
        * ('env, 'a Attribute.t typ) term
        * ('a Attribute.t typ * 'env, 'b) term
        -> ('env, 'b) term

  type _ fmt =
    | Eof : 'env fmt
    | Text : string * 'env fmt -> 'env fmt
    | Int : int * 'env fmt -> 'env fmt

  let substitute symbol attribute command =
    Substitute (symbol, attribute, command)

  let attribute attr = Attribute attr
  let echo s = Echo s
  let echo_attr attr = Echo (AttributeToString attr)

  (* let cmd : (unit, unit typ) term = *)
  (*   let attr = attribute Attribute.Clients_focus_class in *)
  (*   let v = AttributeToString (Var Z) in *)
  (*   let v = ConcatString (String "class=", ConcatString (v, Eof)) in *)
  (*   substitute "T" attr (echo v) *)

  let cmd : (unit, unit typ) term =
    let attr = Var Z in
    let v = AttributeToString (Var Z) in
    let v = ConcatString (String "class=", ConcatString (v, Eof)) in
    let cmd = substitute "T" attr (echo v) in
    let attr = attribute Attribute.Clients_focus_class in
    substitute "S" attr cmd

  let rec lookup_name : type a b. a env -> (a, b) var -> string =
   fun env var ->
    match (env, var) with
    | Nil, _ -> raise (Not_found_s (Atom "Empty stack"))
    | Cons (_, name, _), Z -> name
    | Cons (_, _, env), S var -> lookup_name env var

  let rec eval : type a b. Buffer.t -> a env -> (a, b) term -> unit =
   fun buffer env term ->
    match term with
    | Attribute attr -> Buffer.add_string buffer (Attribute.to_string attr)
    | Int n -> Buffer.add_string buffer (Int.to_string n)
    | Eof -> ()
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
    | AttributeToString term -> eval buffer env term
    | Substitute (name, attr, term) ->
        Buffer.add_string buffer ("substitute " ^ name ^ " ");
        eval buffer env attr;
        Buffer.add_string buffer " ";
        eval buffer (Cons (TAttr, name, env)) term
end
