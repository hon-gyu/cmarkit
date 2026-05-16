module String_map = Map.Make (String)
module Ascii = Cmarkit_base.Ascii
module Text = Cmarkit_base.Text
module Match = Cmarkit_base
module Textloc = Cmarkit_base.Textloc
module Meta = Cmarkit_base.Meta
module Layout = struct
  type blanks = string
  type nonrec string = string
  type nonrec char = char
  type count = int
  type indent = int
  let string ?(meta = Meta.none) s = s, meta
  let empty = string ""
end

type byte_pos = Textloc.byte_pos
type line_span = Match.line_span =
  (* Substring on a single line, hereafter abbreviated to span *)
  { line_pos : Textloc.line_pos; first : byte_pos; last : byte_pos }
type 'a node = 'a * Meta.t

module Block_line = struct
  let _list_of_string flush s = (* cuts [s] on newlines *)
    let rec loop s acc max start k =
      if k > max then List.rev (flush s start max acc) else
      if not (s.[k] = '\n' || s.[k] = '\r')
      then loop s acc max start (k + 1) else
      let acc = flush s start (k - 1) acc in
      let next = k + 1 in
      let start =
        if s.[k] = '\r' && next <= max && s.[next] = '\n' then next + 1 else
        next
      in
      loop s acc max start start
    in
    loop s [] (String.length s - 1) 0 0

  let flush ?(meta = Meta.none) s start last acc =
    let sub = String.sub s start (last - start + 1) in
    (sub, meta) :: acc

  let flush_tight ?(meta = Meta.none) s start last acc =
    (* If [s] has newlines, blanks after newlines are layout *)
    if start > last then ("", ("", meta)) :: acc else
    match acc with
    | [] (* On the first line the blanks are legit *) ->
        ("", (String.sub s start (last - start + 1), meta)) :: acc
    | acc ->
        let nb = Match.first_non_blank s ~last ~start in
        (String.sub s start (nb - 1 - start + 1),
          (String.sub s nb (last - nb + 1), meta)) :: acc

  (* Block lines *)

  type t = string node

  let to_string = fst
  let list_of_string ?meta s = _list_of_string (flush ?meta) s
  let list_textloc = function
  | [] -> Textloc.none | [(_, m)] -> Meta.textloc m
  | (_, first) :: _ as l ->
      let _, last = List.hd (List.rev l) in
      Textloc.reloc ~first:(Meta.textloc first) ~last:(Meta.textloc last)

  (* Tight lines *)

  type tight = Layout.blanks * t

  let tight_to_string l = fst (snd l)
  let tight_list_of_string ?meta s = _list_of_string (flush_tight ?meta) s
  let tight_list_textloc = function
  | [] -> Textloc.none | [_, (_, m)] -> Meta.textloc m
  | (_, (_, first)) :: _ as l ->
      let (_, (_, last)) = List.hd (List.rev l) in
      Textloc.reloc ~first:(Meta.textloc first) ~last:(Meta.textloc last)

  (* Blank lines *)

  type blank = Layout.blanks node
end

module Label = struct
  type key = string
  type t = { meta : Meta.t; key : key; text : Block_line.tight list }
  let make ?(meta = Meta.none) ~key text = { key; text; meta }
  let with_meta meta l = { l with meta }
  let meta t = t.meta
  let key t = t.key
  let text t = t.text
  let textloc t = Block_line.tight_list_textloc t.text
  let text_to_string t =
    String.concat " " (List.map Block_line.tight_to_string t.text)

  let compare l0 l1 = String.compare l0.key l1.key

  (* Definitions *)

  module Map = Map.Make (String)
  type def = ..
  type defs = def Map.t

  (* Resolvers *)

  type context =
  [ `Def of t option * t
  | `Ref of [ `Link | `Image ] * t * (t option) ]

  type resolver = context -> t option
  let default_resolver = function
  | `Def (None, k) -> Some k
  | `Def (Some _, k) -> None
  | `Ref (_, _, k) -> k
end

module Link_definition = struct
  type layout =
    { indent : Layout.indent;
      angled_dest : bool;
      before_dest : Block_line.blank list;
      after_dest : Block_line.blank list;
      title_open_delim : Layout.char;
      after_title : Block_line.blank list; }

  let layout_for_dest dest =
    let needs_angles c = Ascii.is_control c || c = ' ' in
    let angled_dest = String.exists needs_angles dest in
    { indent = 0; angled_dest; before_dest = [];
      after_dest = []; title_open_delim = '\"'; after_title = [] }

  let default_layout =
    { indent = 0; angled_dest = false; before_dest = [];
      after_dest = []; title_open_delim = '\"'; after_title = [] }

  type t =
    { layout : layout;
      label : Label.t option;
      defined_label : Label.t option;
      dest : string node option;
      title : Block_line.tight list option; }

  let make ?layout ?defined_label ?label ?dest ?title () =
    let layout = match dest with
    | None -> default_layout | Some (d, _) -> layout_for_dest d
    in
    let defined_label = match defined_label with None -> label | Some d -> d in
    { layout; label; defined_label; dest; title }

  let layout ld = ld.layout
  let label ld = ld.label
  let defined_label ld = ld.defined_label
  let dest ld = ld.dest
  let title ld = ld.title

  type Label.def += Def of t node
end
