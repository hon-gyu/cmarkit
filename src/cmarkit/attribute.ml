type t =
  { id : string option;
    classes : string list;
    key_values : (string * string) list;
    source : string option }

let empty = { id = None; classes = []; key_values = []; source = None }

(* An attribute carrying no id, classes or key-values, regardless of its
   [source]. A comment-only specifier (e.g. [{% a comment %}]) or a bare
   [{}] parses to such a value: it conveys nothing and, per Djot, is dropped
   from the output rather than attached or rendered literally. *)
let is_empty a = a.id = None && a.classes = [] && a.key_values = []

let id a = a.id
let classes a = a.classes
let key_values a = a.key_values
let source a = a.source

let merge a b =
  (* A key given on both sides takes [b]'s value: merging is an override, not an
     accumulation, so [ {title=foo} ] on a reference definition and [ {title=bar} ]
     on the link that uses it yield one [title], the link's. Classes do
     accumulate, as they do in HTML. *)
  let key_values =
    let overridden (k, _) = List.mem_assoc k b.key_values in
    List.filter (fun kv -> not (overridden kv)) a.key_values @ b.key_values
  in
  { id = (match b.id with None -> a.id | Some _ as id -> id);
    classes = a.classes @ b.classes;
    key_values;
    source = None }

let is_name_char = function
| 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | ':' | '-' -> true
| _ -> false

let of_string s =
  let len = String.length s in
  let rec skip_comment i =
    if i >= len then len else if s.[i] = '%' then i + 1 else skip_comment (i + 1)
  in
  let rec skip_blanks i =
    if i >= len then i else
    match s.[i] with
    | ' ' | '\t' | '\n' | '\r' -> skip_blanks (i + 1)
    | '%' -> skip_blanks (skip_comment (i + 1))
    | _ -> i
  in
  let name i =
    let rec loop k = if k < len && is_name_char s.[k] then loop (k + 1) else k in
    let last = loop i in
    if last = i then None else Some (String.sub s i (last - i), last)
  in
  let quoted i =
    let b = Buffer.create 16 in
    let rec loop k =
      if k >= len then None else
      match s.[k] with
      | '"' -> Some (Buffer.contents b, k + 1)
      | '\\' when k + 1 < len ->
          Buffer.add_char b s.[k + 1]; loop (k + 2)
      | c -> Buffer.add_char b c; loop (k + 1)
    in
    loop i
  in
  let rec loop a i =
    let i = skip_blanks i in
    if i >= len then Some a else
    match s.[i] with
    | '.' ->
        begin match name (i + 1) with
        | None -> None
        | Some (c, next) -> loop { a with classes = a.classes @ [c] } next
        end
    | '#' ->
        begin match name (i + 1) with
        | None -> None
        | Some (id, next) -> loop { a with id = Some id } next
        end
    | _ ->
        begin match name i with
        | None -> None
        | Some (key, eq) when eq < len && s.[eq] = '=' ->
            let value = eq + 1 in
            if value < len && s.[value] = '"' then
              begin match quoted (value + 1) with
              | None -> None
              | Some (v, next) ->
                  loop { a with key_values = a.key_values @ [key, v] } next
              end
            else
              begin match name value with
              | None -> None
              | Some (v, next) ->
                  loop { a with key_values = a.key_values @ [key, v] } next
              end
        | Some _ -> None
        end
  in
  Option.map (fun a -> { a with source = Some s }) (loop empty 0)

let add_escaped_value b value =
  let bare = value <> "" && String.for_all is_name_char value in
  if bare then Buffer.add_string b value else begin
    Buffer.add_char b '"';
    String.iter
      (fun c ->
        if c = '\\' || c = '"' then Buffer.add_char b '\\';
        Buffer.add_char b c)
      value;
    Buffer.add_char b '"'
  end

let to_string a =
  let b = Buffer.create 32 in
  let sep () = if Buffer.length b > 0 then Buffer.add_char b ' ' in
  begin match a.id with
  | None -> ()
  | Some id -> Buffer.add_char b '#'; Buffer.add_string b id
  end;
  List.iter
    (fun c -> sep (); Buffer.add_char b '.'; Buffer.add_string b c)
    a.classes;
  List.iter
    (fun (key, value) ->
      sep (); Buffer.add_string b key; Buffer.add_char b '=';
      add_escaped_value b value)
    a.key_values;
  Buffer.contents b
