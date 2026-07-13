open Cmarkit_
module G = QCheck2.Gen

let text_egs : Inline.t list =
  [ "jia"; "yi"; "bing" ] |> List.map (fun pl -> Inline.(Text (pl, Meta.none)))

open struct
  (* [of_string] takes the code span's *content* and computes the fence width. *)
  let plain_code_span_egs : Inline.t list =
    Inline.Code_span.[ of_string "add"; of_string "sub"; of_string "mul" ]
    |> List.map (fun pl -> Inline.(Code_span (pl, Meta.none)))

  (* Code spans whose content contains backticks. Built directly rather than via
   [of_string], whose fence-width computation ([min_backtick_count]) is buggy
   for repeated run-lengths. The fence must be a backtick run longer than, and
   not equal to, any run inside the content; content that starts or ends with a
   backtick is padded with one space (stripped again on parse). *)
  let backtick_code_span_egs : Inline.t list =
    let mk ~backtick_count content =
      Inline.(
        Code_span
          ( Code_span.make ~backtick_count
              (Block_line.tight_list_of_string content),
            Meta.none ))
    in
    [ mk ~backtick_count:2 "a`b"; mk ~backtick_count:2 " `mul` " ]
end

let code_span_egs : Inline.t list = plain_code_span_egs @ backtick_code_span_egs

let (autolink_egs : Inline.t list) =
  Inline.Autolink.(
    (* TODO(rule): URI autolinks need a scheme; [<www.foo.com>] is literal text, not an
       autolink. Emails are recognized as-is. *)
    [
      make ("http://www.foo.com", Meta.none); make ("bar@gmail.com", Meta.none);
    ])
  |> List.map (fun pl -> Inline.(Autolink (pl, Meta.none)))

let (break_egs : Inline.t list) =
  Inline.Break.[ make `Hard; make `Soft ]
  |> List.map (fun pl -> Inline.(Break (pl, Meta.none)))

(* [marked] sets opener/closer markers ([{_]…[_}]) on every emphasis. Two
   emphasis spans rendered flush against each other ([_a__b_]) otherwise fuse
   into one on reparse; markers ([{_a_}{_b_}]) keep the boundary explicit. We
   mark unconditionally rather than only at fusing adjacencies because adjacency
   can be transitive through a nested [Inlines], and markers are invisible to
   the structural AST comparison. Requires the parser's [marked_emphasis_delims]
   to be enabled. *)
let mk_emph_egs ?(delims = [ '*'; '_' ]) ?(marked = false) () i : Inline.t list
    =
  if not (List.for_all (fun c -> List.mem c [ '*'; '_' ]) delims) then
    raise (Invalid_argument "Delim must be among ['*'; '_']");
  Inline.Emphasis.(
    List.map
      (fun c -> make ~delim:c ~open_marker:marked ~close_marker:marked i)
      delims)
  |> List.map (fun pl -> Inline.(Emphasis (pl, Meta.none)))

let mk_strong_emph_egs ?(delims = [ '*'; '_' ]) ?(marked = false) () i :
    Inline.t list =
  if not (List.for_all (fun c -> List.mem c [ '*'; '_' ]) delims) then
    raise (Invalid_argument "Delim must be among ['*'; '_']");
  Inline.Emphasis.(
    List.map
      (fun c -> make ~delim:c ~open_marker:marked ~close_marker:marked i)
      delims)
  |> List.map (fun pl -> Inline.(Strong_emphasis (pl, Meta.none)))

let mk_link i : Inline.t =
  let dest = ("https://example.com", Meta.none) in
  let ref_ = `Inline (Link_definition.make ~dest (), Meta.none) in
  Inline.(Link (Inline.Link.make i ref_, Meta.none))

let mk_image i : Inline.t =
  let dest = ("https://example.com/img.png", Meta.none) in
  let ref_ = `Inline (Link_definition.make ~dest (), Meta.none) in
  Inline.(Image (Inline.Link.make i ref_, Meta.none))

(** @note{[
    There was "<em>foo</em>" but removed because:
    Inline raw HTML is one tag (or comment) per node: the parser emits [<em>],
    text, then [</em>] as three inlines, never a whole element in one node.
    ]}
 *)
let (raw_html_egs : Inline.t list) =
  [ "<br />"; "<em>"; "</em>"; "<!-- comment -->" ]
  |> List.map (fun s ->
      Inline.(Raw_html (Block_line.tight_list_of_string s, Meta.none)))

(* TODO: extension strikethrough and math_span *)

(* Djot symbols. Opaque leaves: no children, never span a line. They need no
   separation rule — [:a:] and [:b:] flush against each other render [:a::b:],
   and the scanner takes [:a:] then [:b:], recovering both. Requires the
   parser's [djot_symbols]. *)
let symbol_egs : Inline.t list =
  [ "smile"; "+1"; "a_b" ]
  |> List.map (fun n -> Inline.(Ext_symbol (Symbol.make n, Meta.none)))

let mk_smart_punct ?(marker = false) kind =
  Inline.(Ext_smart_punct (Smart_punct.make ~marker kind, Meta.none))

(* [marked] puts braces on every quote ([{"]…["}]), for the same reason
   [marked_emphasis] does on emphasis: a quote's direction is *inferred from its
   neighbours*, so a bare quote reparsed in a different neighbourhood can curl
   the other way. The marker states the direction outright. Requires the
   parser's [smart_punctuation]. *)
let smart_quote_egs ~marked : Inline.t list =
  Inline.Smart_punct.
    [ Left_double_quote; Right_double_quote;
      Left_single_quote; Right_single_quote ]
  |> List.map (mk_smart_punct ~marker:marked)

(* An ellipsis needs no rule: [...] flush against [...] is six periods, which
   divides back into exactly two ellipses. Dashes are the hazard — see
   [no_adjacent_smart_dashes]. *)
let smart_dash_egs : Inline.t list =
  Inline.Smart_punct.[ Em_dash; En_dash ] |> List.map (fun k -> mk_smart_punct k)

let smart_ellipsis_egs : Inline.t list = [ mk_smart_punct Inline.Smart_punct.Ellipsis ]

(* A run of hyphens is divided by its *total length* alone, so the parser cannot
   recover how the AST split it. [En_dash] then [Em_dash] renders [-----], which
   divides back into em-then-en: the order flips. Even a uniform run is unsafe —
   three [En_dash] render six hyphens, which come back as two [Em_dash]. So no
   two dash nodes may render flush; unlike emphasis there is no marker escape.
   Mirrors [drop_fusing_code_spans]. *)
let rec starts_with_smart_dash = function
  | Inline.Ext_smart_punct (sp, _) ->
      (match Inline.Smart_punct.kind sp with
       | Inline.Smart_punct.Em_dash | Inline.Smart_punct.En_dash -> true
       | _ -> false)
  | Inline.Inlines (i :: _, _) -> starts_with_smart_dash i
  | _ -> false

let rec ends_with_smart_dash = function
  | Inline.Ext_smart_punct _ as i -> starts_with_smart_dash i
  | Inline.Inlines (is, _) -> (
      match List.rev is with
      | last :: _ -> ends_with_smart_dash last
      | [] -> false)
  | _ -> false

let drop_fusing_smart_dashes (is : Inline.t list) : Inline.t list =
  let rec loop trailing_dash acc = function
    | [] -> List.rev acc
    | e :: es ->
        let n = Inline.normalize e in
        if Inline.is_empty n then loop trailing_dash (e :: acc) es
        else if trailing_dash && starts_with_smart_dash n then
          loop trailing_dash acc es
        else loop (ends_with_smart_dash n) (e :: acc) es
  in
  loop false [] is

(* Code spans have no witness when rendered flush against each other: the
   closing backtick fence of one and the opening fence of the next merge into a
   single longer run, which can't match either fence length, so the parser reads
   a single span (or literal backticks) — never two adjacent ones. Unlike
   emphasis there is no marker escape, so the only sound move is to not place
   them adjacently. [starts_with_code_span]/[ends_with_code_span] detect a bare
   code-span fence at an inline's rendered boundary (descending into the
   first/last child of an [Inlines]; emphasis/links/etc. shield their content
   behind their own delimiters, so they never expose one). They run on the
   {e normalized} element, which (with normalize now flattening fully) is a flat
   list with rendered-empty filler spliced away. *)
let rec starts_with_code_span = function
  | Inline.Code_span _ -> true
  | Inline.Inlines (i :: _, _) -> starts_with_code_span i
  | _ -> false

let rec ends_with_code_span = function
  | Inline.Code_span _ -> true
  | Inline.Inlines (is, _) -> (
      match List.rev is with last :: _ -> ends_with_code_span last | [] -> false)
  | _ -> false

(* Drop any element that would render flush against a preceding code span. An
   element that normalizes to empty renders to nothing (the parser sees straight
   through it), so it neither separates nor fuses. The original (un-normalized)
   element is kept, preserving nesting variety in the corpus. *)
let drop_fusing_code_spans (is : Inline.t list) : Inline.t list =
  let rec loop trailing_cs acc = function
    | [] -> List.rev acc
    | e :: es ->
        let n = Inline.normalize e in
        if Inline.is_empty n then loop trailing_cs (e :: acc) es
        else if trailing_cs && starts_with_code_span n then loop trailing_cs acc es
        else loop (ends_with_code_span n) (e :: acc) es
  in
  loop false [] is

module Iconfig = struct
  type t = {
    w_text : int;
    w_code_span : int;
    w_autolink : int;
    w_break : int;
    w_raw_html : int;
    w_inlines : int;
    w_emphasis : int;
    w_strong_emphasis : int;
    w_link : int;
    w_image : int;
    w_symbol : int;
        (** Djot symbols. Defaults to 0: the node only has a witness when the
            parser's [djot_symbols] is on, so a consumer must opt in on both
            sides. *)
    w_smart_punct : int;
        (** Djot smart punctuation. Defaults to 0, and pairs with the parser's
            [smart_punctuation] the same way [w_symbol] does. *)
    no_empty_inlines : bool;
    no_empty_emphasis : bool;
    no_html_block_start : bool;
    no_nested_link : bool;
        (** When set, a [Link] never contains another [Link] at any nesting
            depth (even across an intervening image). This is explicitly
            required in Commonmark Spec *)
    different_delim_char_for_emph_and_strong_empha : bool;
        (** Avoid the ambiguity in parsing emphasis/strong emphasis runs.
            Otherwise, the following AST has no witness markdown: (Paragraph
            (Emphasis (Emphasis (Emphasis (Text jia))))) `***jia***` It will
            always to parsed to Emphasis (Emphasis (Text jia)) *)
    marked_emphasis : bool;
        (** Emit opener/closer markers ([{_]…[_}]) on every emphasis and strong
            emphasis. Without this, two emphasis spans that end up flush against
            each other ([_a__b_]) fuse into a single span on reparse, so the AST
            (Inlines (Emphasis ...) (Emphasis ...)) has no witness. With markers
            ([{_a_}{_b_}]) the boundary is explicit. Pairs with the parser's
            [marked_emphasis_delims]; consumers must enable that knob when
            reparsing. *)
    no_adjacent_code_spans : bool;
        (** Never place a code span flush against a preceding one within an
            [Inlines]. Two adjacent code spans have no CommonMark witness — their
            backtick fences merge into a single run — and there is no marker
            escape as there is for emphasis. *)
    no_adjacent_smart_dashes : bool;
        (** Never place a smart-punctuation dash flush against a preceding one.
            A hyphen run is divided by its total length alone, so the split the
            AST intended is not recoverable: [En_dash] then [Em_dash] renders
            [-----] and comes back em-then-en (the order flips), and three
            [En_dash] render six hyphens, which come back as two [Em_dash]. Even
            uniform runs are unsafe, and there is no marker escape. *)
    marked_smart_quotes : bool;
        (** Put braces on every smart quote ([{"]…["}]). A quote's direction is
            inferred from its neighbours, so a bare quote can curl the other way
            when reparsed in a different neighbourhood; the marker states the
            direction outright. Pairs with the parser's [smart_punctuation],
            exactly as [marked_emphasis] pairs with [marked_emphasis_delims]. *)
  }

  let default =
    {
      w_text = 1;
      w_code_span = 1;
      w_autolink = 1;
      w_break = 1;
      w_raw_html = 1;
      w_inlines = 1;
      w_emphasis = 1;
      w_strong_emphasis = 1;
      w_link = 1;
      w_image = 1;
      w_symbol = 0;
      w_smart_punct = 0;
      no_empty_inlines = false;
      no_empty_emphasis = false;
      no_html_block_start = false;
      no_nested_link = false;
      different_delim_char_for_emph_and_strong_empha = false;
      marked_emphasis = false;
      no_adjacent_code_spans = false;
      no_adjacent_smart_dashes = false;
      marked_smart_quotes = false;
    }

  let typed =
    {
      default with
      no_empty_emphasis = true;
      no_nested_link = true;
      different_delim_char_for_emph_and_strong_empha = true;
      marked_emphasis = true;
      no_adjacent_code_spans = true;
      no_adjacent_smart_dashes = true;
      marked_smart_quotes = true;
    }

  (** {!typed} with the djot inline extensions switched on. Kept separate
      because the nodes only have a witness when the parser is given
      [djot_symbols] and [smart_punctuation]; reparsing without those knobs
      turns the rendered source back into plain text. *)
  let typed_djot = { typed with w_symbol = 1; w_smart_punct = 1 }
end

(**
   @note{[
   [Break] is intentionally not a leaf: it is a connective with no standalone
   CommonMark witness (a lone/trailing break renders to whitespace and is
   dropped on reparse). It belongs strictly between two non-break inlines; see
   [w_break] for where it is reintroduced.
   ]}
 *)
let gen_leaf (ic : Iconfig.t) =
  let smart_punct_egs =
    smart_quote_egs ~marked:ic.marked_smart_quotes
    @ smart_dash_egs @ smart_ellipsis_egs
  in
  [
    (ic.w_text, G.oneof_list text_egs);
    (ic.w_code_span, G.oneof_list code_span_egs);
    (ic.w_autolink, G.oneof_list autolink_egs);
    ( (if ic.no_html_block_start then 0 else ic.w_raw_html),
      G.oneof_list raw_html_egs );
    (ic.w_symbol, G.oneof_list symbol_egs);
    (ic.w_smart_punct, G.oneof_list smart_punct_egs);
  ]
  |> List.filter (fun (w, _) -> w > 0)
  |> G.oneof_weighted

let gen_inline (ic : Iconfig.t) : Inline.t G.t =
  let open Iconfig in
  let open G in
  let inlines_len ic n =
    if ic.no_empty_inlines then int_range 1 (max 1 (n / 2))
    else int_bound (n / 2)
  in
  let marked = ic.marked_emphasis in
  let mk_emph_egs, mk_strong_emph_egs =
    if ic.different_delim_char_for_emph_and_strong_empha then
      ( mk_emph_egs ~delims:[ '_' ] ~marked (),
        mk_strong_emph_egs ~delims:[ '*' ] ~marked () )
    else (mk_emph_egs ~marked (), mk_strong_emph_egs ~marked ())
  in
  (* [ic] is threaded through the recursion (à la [gen.ml]'s [config]) so a
     subtree can be generated under a tightened config.

     Each recursive sub-generator goes through [delay], which is load-bearing:
     it hands back the generator without running the recursion, so building a
     node is O(branches) and only the one branch [oneof_weighted] picks unfolds
     at run time. Calling [self] directly (an eager [let rec] over [n - 1])
     would build all children at every node and blow up exponentially. *)
  let rec self (ic, n) =
    let child ic' k = delay (fun () -> self (ic', k)) in
    match n with
    | 0 -> gen_leaf ic
    | n ->
        let inlines_of_is is =
          let is = if ic.no_adjacent_code_spans then drop_fusing_code_spans is else is in
          let is =
            if ic.no_adjacent_smart_dashes then drop_fusing_smart_dashes is
            else is
          in
          Inline.Inlines (is, Meta.none)
        in
        let emphasis_ic =
          if ic.no_empty_emphasis then
            { ic with no_empty_inlines = true; no_empty_emphasis = true }
          else ic
        in
        let emph_gen =
          bind
            (child emphasis_ic (n - 1))
            (fun i -> oneof_list @@ mk_emph_egs i)
        in
        let strong_emph_gen =
          bind
            (child emphasis_ic (n - 1))
            (fun i -> oneof_list @@ mk_strong_emph_egs i)
        in
        (* CommonMark forbids a link inside a link at any depth. When
           [no_nested_link] is set, generate link content with links disabled
           all the way down by threading [w_link = 0] into it. *)
        let link_ic =
          if ic.no_nested_link then { ic with w_link = 0 } else ic
        in
        [
          (1, gen_leaf ic);
          ( ic.w_inlines,
            let gen_is =
              if ic.no_html_block_start then
                let first = child ic (n / 2) in
                let rest =
                  list_size
                    (int_bound (n / 2))
                    (child { ic with no_html_block_start = false } (n / 2))
                in
                map2 (fun i is -> i :: is) first rest
              else list_size (inlines_len ic n) (child ic (n / 2))
            in
            map inlines_of_is gen_is );
          (ic.w_emphasis, emph_gen);
          (ic.w_strong_emphasis, strong_emph_gen);
          (ic.w_link, map mk_link (child link_ic (n - 1)));
          (ic.w_image, map mk_image (child ic (n - 1)));
        ]
        |> List.filter (fun (w, _) -> w > 0)
        |> oneof_weighted
  in
  sized_size nat_small (fun n -> self (ic, n))

let%expect_test "Default config should give a sensible distribution" =
  Pp_distr.pp_gen () Format.std_formatter
    (gen_inline Iconfig.default)
    Stat.inline_stats;
  [%expect
    {|
                                             Boxplot
    ┌──────────────────────────┬────────────────────────────────────────────────────────────┐
    │n=1000                    │↓0                                                       17↓│
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │text                      │[------------+---------------------------------------------~│
    │p5=0.00|p95=18.00|mu=4.01 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_span                 │[-------------+--------------------------------------------~│
    │p5=0.00|p95=19.00|mu=4.07 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │autolink                  │[-------------+--------------------------------------------~│
    │p5=0.00|p95=19.00|mu=4.10 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │break                     │+                                                           │
    │p5=0.00|p95=0.00|mu=0.00  │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │raw_html                  │[-------------+--------------------------------------------~│
    │p5=0.00|p95=18.00|mu=4.09 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │emphasis                  │[------------------------------------------+---------------~│
    │p5=0.00|p95=60.00|mu=12.58│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │strong_emphasis           │[------------------------------------------+---------------~│
    │p5=0.00|p95=56.00|mu=12.48│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │link                      │[------------------------------------------+---------------~│
    │p5=0.00|p95=60.00|mu=12.57│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │image                     │[------------------------------------------+---------------~│
    │p5=0.00|p95=55.00|mu=12.68│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │inlines                   │[------------------------------------------+---------------~│
    │p5=0.00|p95=55.00|mu=12.43│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │symbol                    │+                                                           │
    │p5=0.00|p95=0.00|mu=0.00  │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │smart_punct               │+                                                           │
    │p5=0.00|p95=0.00|mu=0.00  │                                                            │
    └──────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]
