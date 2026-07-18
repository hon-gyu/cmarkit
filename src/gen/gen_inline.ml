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
   parser's [colon_symbols]. *)
let symbol_egs : Inline.t list =
  [ "smile"; "+1"; "a_b" ]
  |> List.map (fun n -> Inline.(Ext_symbol (Symbol.make n, Meta.none)))

let mk_smart_punct ?(marker = false) kind =
  Inline.(Ext_smart_punct (Smart_punct.make ~marker kind, Meta.none))

(* Quoted spans. A quote is a delimiter: the parser pairs an opener with a closer
   and the pair becomes [Ext_quoted]; only a quote that fails to pair is left as
   a bare [Smart_punct] character. So the *container* is what the generator can
   place, and a bare quote character is what it cannot — see [no_bare_smart_quotes].

   [marked] puts braces on the delimiters ([{"]…["}]), for the same reason
   [marked_emphasis] does on emphasis, and it buys the same two things.

   It disambiguates nesting: bare [ "jia"jia"" ] is a quoted span holding a
   quoted span at its end, but the parser pairs the first closer it meets with
   the opener on top of the stack, and reads the pair the other way round.
   Marked delimiters pair only with marked delimiters ([{"] with ["}]), so the
   boundary is explicit.

   It also lifts djot's [canOpen] restriction on a [Single] opener, which
   otherwise only opens at the start of a line or after one of a small set of
   characters (space, [-], [(], [[], a quote). Unmarked, whether a single-quoted
   span roundtrips depends on the *rendered text to its left* — [ 'x' ] after a
   word renders [ jia'x' ], whose quotes open nothing and come back two
   apostrophes — and that is a property of the whole prefix, not of the node or
   its neighbour, which the generator has no local way to state. So unmarked,
   only [Double] is generated; marked, both kinds are. *)
let mk_quoted_egs ~marked i =
  let kinds =
    if marked then Inline.Quoted.[ Single; Double ] else [ Inline.Quoted.Double ]
  in
  kinds
  |> List.map (fun kind ->
         Inline.(
           Ext_quoted
             ( Quoted.make ~open_marker:marked ~close_marker:marked kind i,
               Meta.none )))

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
    w_quoted : int;
        (** Weight of an {!Inline.Ext_quoted} span. Requires the parser's
            [smart_punctuation]. *)
        (** Djot symbols. Defaults to 0: the node only has a witness when the
            parser's [colon_symbols] is on, so a consumer must opt in on both
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
    marked_quotes : bool;
        (** Put braces on every quoted span's delimiters ([{"]…["}]). Quotes
            pair like emphasis, so they have emphasis's nesting ambiguity —
            [ "a"b"" ] is read with the boundary in the wrong place — and marked
            delimiters pair only with marked ones, which states it. It also
            frees a single-quoted span from djot's [canOpen] restriction, which
            otherwise makes its roundtrip depend on the text rendered to its
            left. Pairs with the parser's [smart_punctuation]. *)
    no_bare_smart_quotes : bool;
        (** Never generate a bare quote character ([Left_double_quote] and the
            three other quote kinds of [Smart_punct]). A quote is a delimiter,
            not a character: the parser pairs openers with closers, and a bare
            quote node is only what is *left over* when a quote fails to pair.
            So a bare quote is not compositionally placeable — two of them in a
            paragraph pair up and come back as one [Ext_quoted], and a lone
            [Right_double_quote] renders a quote that has nothing to pair with
            and comes back a *left* one (an unmatched double quote falls back to
            the opening direction, an unmatched single one to an apostrophe).

            Quoted spans are generated as [Ext_quoted] containers instead, which
            do roundtrip; see [w_quoted]. *)
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
      w_quoted = 0;
      no_empty_inlines = false;
      no_empty_emphasis = false;
      no_html_block_start = false;
      no_nested_link = false;
      different_delim_char_for_emph_and_strong_empha = false;
      marked_emphasis = false;
      no_adjacent_code_spans = false;
      no_adjacent_smart_dashes = false;
      marked_quotes = false;
      no_bare_smart_quotes = false;
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
      marked_quotes = true;
      no_bare_smart_quotes = true;
    }

  (** {!typed} with the djot inline extensions switched on. Kept separate
      because the nodes only have a witness when the parser is given
      [colon_symbols] and [smart_punctuation]; reparsing without those knobs
      turns the rendered source back into plain text. *)
  let typed_djot = { typed with w_symbol = 1; w_smart_punct = 1; w_quoted = 1 }
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
    let quotes =
      if ic.no_bare_smart_quotes then []
      else
        Inline.Smart_punct.
          [ Left_double_quote; Right_double_quote;
            Left_single_quote; Right_single_quote ]
        |> List.map (fun k -> mk_smart_punct k)
    in
    quotes @ smart_dash_egs @ smart_ellipsis_egs
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
          (* An empty span is not a span: [ "" ] is an empty pair, which djot
             does not match (it is the rule that lets [ ''hi'' ] nest), so the
             two quotes come back as bare characters. *)
          ( ic.w_quoted,
            bind
              (child emphasis_ic (n - 1))
              (fun i -> oneof_list @@ mk_quoted_egs ~marked:ic.marked_quotes i) );
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
