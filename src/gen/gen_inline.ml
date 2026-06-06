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
      no_empty_inlines = false;
      no_empty_emphasis = false;
      no_html_block_start = false;
      no_nested_link = false;
      different_delim_char_for_emph_and_strong_empha = false;
      marked_emphasis = false;
    }

  let typed =
    {
      default with
      no_empty_emphasis = true;
      no_nested_link = true;
      different_delim_char_for_emph_and_strong_empha = true;
      marked_emphasis = true;
    }
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
  [
    (ic.w_text, G.oneof_list text_egs);
    (ic.w_code_span, G.oneof_list code_span_egs);
    (ic.w_autolink, G.oneof_list autolink_egs);
    ( (if ic.no_html_block_start then 0 else ic.w_raw_html),
      G.oneof_list raw_html_egs );
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
        let inlines_of_is is = Inline.Inlines (is, Meta.none) in
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
    │n=1000                    │↓0                                                       28↓│
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │text                      │[-------+----------------------------]                      │
    │p5=0.00|p95=18.00|mu=4.01 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_span                 │[-------+-------------------------------]                   │
    │p5=0.00|p95=19.00|mu=4.07 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │autolink                  │[-------+-------------------------------]                   │
    │p5=0.00|p95=19.00|mu=4.10 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │break                     │+                                                           │
    │p5=0.00|p95=0.00|mu=0.00  │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │raw_html                  │[-------+----------------------------]                      │
    │p5=0.00|p95=18.00|mu=4.09 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │emphasis                  │[-------------------------+--------------------------------~│
    │p5=0.00|p95=60.00|mu=12.58│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │strong_emphasis           │[-------------------------+--------------------------------~│
    │p5=0.00|p95=56.00|mu=12.48│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │link                      │[-------------------------+--------------------------------~│
    │p5=0.00|p95=60.00|mu=12.57│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │image                     │[-------------------------+--------------------------------~│
    │p5=0.00|p95=55.00|mu=12.68│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │inlines                   │[-------------------------+--------------------------------~│
    │p5=0.00|p95=55.00|mu=12.43│                                                            │
    └──────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]
