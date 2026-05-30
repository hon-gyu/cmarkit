open Oymarkit_
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

let mk_emph_egs i : Inline.t list =
  Inline.Emphasis.[ make ~delim:'*' i; make ~delim:'_' i ]
  |> List.map (fun pl -> Inline.(Emphasis (pl, Meta.none)))

let mk_strong_emph_egs i : Inline.t list =
  Inline.Emphasis.[ make ~delim:'*' i; make ~delim:'_' i ]
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
    (ic.w_raw_html, G.oneof_list raw_html_egs);
  ]
  |> List.filter (fun (w, _) -> w > 0)
  |> G.oneof_weighted

let gen_inline (ic : Iconfig.t) : Inline.t G.t =
  let open Iconfig in
  let gen_leaf = gen_leaf ic in
  let inlines_len n =
    if ic.no_empty_inlines then G.int_range 1 (max 1 (n / 2))
    else G.int_bound (n / 2)
  in
  G.(
    sized_size nat_small
    @@ fix (fun self (n : int) ->
        match n with
        | 0 -> gen_leaf
        | n ->
            let inlines_of_is is = Inline.Inlines (is, Meta.none) in
            let emph_gen =
              bind (self (n - 1)) (fun i -> oneof_list @@ mk_emph_egs i)
            in
            let strong_emph_gen =
              bind (self (n - 1)) (fun i -> oneof_list @@ mk_strong_emph_egs i)
            in
            [
              (1, gen_leaf);
              ( ic.w_inlines,
                map inlines_of_is (list_size (inlines_len n) (self (n / 2))) );
              (ic.w_emphasis, emph_gen);
              (ic.w_strong_emphasis, strong_emph_gen);
              (ic.w_link, map mk_link (self (n - 1)));
              (ic.w_image, map mk_image (self (n - 1)));
            ]
            |> List.filter (fun (w, _) -> w > 0)
            |> oneof_weighted))

let%expect_test _ =
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
