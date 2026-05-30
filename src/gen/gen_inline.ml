open Oymarkit_
module G = QCheck2.Gen

let text_egs : Inline.t list =
  [ "jia"; "yi"; "bing" ] |> List.map (fun pl -> Inline.(Text (pl, Meta.none)))

let (code_span_egs : Inline.t list) =
  Inline.Code_span.
    [
      (* TODO(rule): No empty code span: [``] renders as literal backticks, not a span. *)
      of_string "`add`";
      of_string "``sub``";
      of_string "`` `mul` ``";
    ]
  |> List.map (fun pl -> Inline.(Code_span (pl, Meta.none)))

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

let (raw_html_egs : Inline.t list) =
  [ "<br />"; "<em>foo</em>"; "<!-- comment -->" ]
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
    nonempty : bool;
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
      nonempty = false;
    }
end

let gen_inline (ic : Iconfig.t) : Inline.t G.t =
  let open Iconfig in
  let gen_leaf =
    [
      (ic.w_text, G.oneof_list text_egs);
      (ic.w_code_span, G.oneof_list code_span_egs);
      (ic.w_autolink, G.oneof_list autolink_egs);
      (ic.w_break, G.oneof_list break_egs);
      (ic.w_raw_html, G.oneof_list raw_html_egs);
    ]
    |> List.filter (fun (w, _) -> w > 0)
    |> G.oneof_weighted
  in
  let inlines_len n =
    if ic.nonempty then G.int_range 1 (max 1 (n / 2)) else G.int_bound (n / 2)
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
    │n=1000                    │↓0                                                       30↓│
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │text                      │[-----+------------------------]                            │
    │p5=0.00|p95=16.00|mu=3.32 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_span                 │[-----+----------------------]                              │
    │p5=0.00|p95=15.00|mu=3.21 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │autolink                  │[-----+--------------------]                                │
    │p5=0.00|p95=14.00|mu=3.22 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │break                     │[-----+------------------------]                            │
    │p5=0.00|p95=16.00|mu=3.25 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │raw_html                  │[-----+----------------------]                              │
    │p5=0.00|p95=15.00|mu=3.28 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │emphasis                  │[-----------------------+----------------------------------~│
    │p5=0.00|p95=60.00|mu=12.58│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │strong_emphasis           │[-----------------------+----------------------------------~│
    │p5=0.00|p95=56.00|mu=12.48│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │link                      │[-----------------------+----------------------------------~│
    │p5=0.00|p95=60.00|mu=12.57│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │image                     │[-----------------------+----------------------------------~│
    │p5=0.00|p95=55.00|mu=12.68│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │inlines                   │[-----------------------+----------------------------------~│
    │p5=0.00|p95=55.00|mu=12.43│                                                            │
    └──────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]
