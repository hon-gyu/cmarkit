(** {0:distr Visualise distributions} *)

module B = PrintBox

let spf = Printf.sprintf

type 'a stat = string * ('a -> float)

let percentile (sorted : float array) (p : float) : float =
  let n = Array.length sorted in
  let i = int_of_float (float_of_int n *. p) in
  sorted.(Int.min i (n - 1))

let std_dev (arr : float array) (mean_val : float) : float =
  let n = Array.length arr in
  let variance =
    Array.fold_left (fun acc x -> acc +. ((x -. mean_val) ** 2.0)) 0.0 arr
    /. float_of_int n
  in
  sqrt variance

(** Two-line axis ruler: [^] tick markers at the scale endpoints, then the
    numeric labels beneath them. Prefixes the left side with [n=...]. *)
let axis_header ~(width : int) ~(scale_lo : float) ~(scale_hi : float) : B.t =
  let lo_str = Printf.sprintf "%.4g" scale_lo in
  let hi_str = Printf.sprintf "%.4g" scale_hi in
  let gap = width - String.length lo_str - String.length hi_str - 2 in
  let axis_str = spf "↓%s%s%s↓" lo_str (String.make gap ' ') hi_str in
  B.text axis_str

(** Draw a box-plot row of [width] chars given float values and a scale. Whisker
    endpoints use [~] when clipped outside the scale. Point markers ([+] mean,
    [*] med, [<] q1, [>] q3) are omitted when out of range. *)
let render_boxplot ~width ~scale_lo ~scale_hi ~lo ~hi ?mean ?med ?q1 ?q3 () =
  let range = scale_hi -. scale_lo in
  let col v =
    if range = 0.0 then width / 2
    else int_of_float ((v -. scale_lo) *. float_of_int (width - 1) /. range)
  in
  let col_clamp v = Int.max 0 (Int.min (width - 1) (col v)) in
  let line = Bytes.make width ' ' in
  let lo_col = col_clamp lo and hi_col = col_clamp hi in
  Bytes.fill line (lo_col + 1) (hi_col - lo_col - 1) '-';
  Bytes.set line lo_col (if lo < scale_lo then '~' else '[');
  Bytes.set line hi_col (if hi > scale_hi then '~' else ']');
  let set_if_visible v c =
    let p = col v in
    if p >= 0 && p < width then Bytes.set line p c
  in
  (match q1 with
  | Some v -> set_if_visible v '<'
  | None -> ());
  (match q3 with
  | Some v -> set_if_visible v '>'
  | None -> ());
  (match mean with
  | Some v -> set_if_visible v '+'
  | None -> ());
  (match med with
  | Some v -> set_if_visible v '*'
  | None -> ());
  Bytes.to_string line

(** Render one boxplot row. [scale_lo]/[scale_hi] override the axis endpoints
    for column mapping (universal scale across multiple stats); when absent the
    per-stat [lo_p]/[hi_p] percentiles define the axis. *)
let boxplot_of_samples_row ?(width = 88) ?(max = true) ?(min = true)
    ?(mean = true) ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p
    ?scale_lo ?scale_hi (samples : float list) : string * string =
  let arr = Array.of_list samples in
  Array.sort Float.compare arr;
  let n = Array.length arr in
  let lo = percentile arr lo_p in
  let hi = percentile arr hi_p in
  let median = percentile arr 0.5 in
  let mean_val = Array.fold_left ( +. ) 0.0 arr /. float_of_int n in
  let q1 = Option.map (percentile arr) q1_p in
  let q3 = Option.map (percentile arr) q3_p in
  let slo = Option.value ~default:lo scale_lo in
  let shi = Option.value ~default:hi scale_hi in
  let plot =
    render_boxplot ~width ~scale_lo:slo ~scale_hi:shi ~lo ~hi ?q1 ?q3
      ?mean:(if mean then Some mean_val else None)
      ?med:(if med then Some median else None)
      ()
  in
  let (stats : string) =
    let int_of_p (p : float) : int =
      p *. 100.0 |> Float.round |> int_of_float
    in
    let parts =
      List.filter_map Fun.id
        [
          (if min then Some (spf "p%d=%-3.2f" (int_of_p lo_p) lo) else None);
          (if max then Some (spf "p%d=%-3.2f" (int_of_p hi_p) hi) else None);
          (if mean then Some (spf "mu=%-3.2f" mean_val) else None);
          (if med then Some (spf "med=%-3.2f" median) else None);
          Option.map (spf "Q1=%-3.2f") q1;
          Option.map (spf "Q3=%-3.2f") q3;
        ]
    in
    String.concat "|" parts
  in
  (stats, plot)

let boxplot_of_samples ?(width = 88) ?(max = true) ?(min = true) ?(mean = true)
    ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p
    ?(scale_lo : float option) ?(scale_hi : float option) (n : int)
    (stat_values : (string * float list) list) : B.t =
  (* Substract label width *)
  let label_width = 20 in
  let value_width = width - label_width in
  let slo, shi =
    match (scale_lo, scale_hi) with
    | Some lo, Some hi -> (lo, hi)
    | _ ->
        let all_arr = Array.of_list (List.concat_map snd stat_values) in
        Array.sort Float.compare all_arr;
        let slo = percentile all_arr lo_p in
        let shi = percentile all_arr hi_p in
        (Option.value ~default:slo scale_lo, Option.value ~default:shi scale_hi)
  in
  let stats_str_lst, box_str_lst =
    List.map
      (fun (_label, values) ->
        boxplot_of_samples_row ~width:value_width ~max ~min ~mean ~med ~lo_p
          ~hi_p ?q1_p ?q3_p ~scale_lo:slo ~scale_hi:shi values)
      stat_values
    |> List.split
  in
  let (lt_header : B.t) = B.text (spf "n=%-*d" label_width n) in
  let (rt_header : B.t) =
    axis_header ~width:value_width ~scale_lo:slo ~scale_hi:shi
  in
  let (header : B.t array) = [| lt_header; rt_header |] in
  let (ld_labels : B.t list) =
    let labels = stat_values |> List.map fst in
    List.map2
      (fun label stat -> B.text (spf "%-*s\n%s" label_width label stat))
      labels stats_str_lst
  in
  let (rd_boxes : B.t list) = List.map (fun s -> B.text s) box_str_lst in
  let (boxes : B.t array array) =
    List.map2 (fun label box -> [| label; box |]) ld_labels rd_boxes
    |> Array.of_list
  in
  let grid_items = Array.append [| header |] boxes in
  B.frame @@ B.grid ~bars:true grid_items

(** Render all stats as a describe()-style table (n, mean, std, min, pLO, q1,
    med, q3, pHI, max). *)
let table_of_samples ?(lo_p = 0.05) ?(hi_p = 0.95)
    (rows : (string * float list) list) : B.t =
  let fmt v = Printf.sprintf "%-4.4g" v in
  let header =
    List.map B.text
      [
        "";
        "n";
        "mean";
        "std";
        "min";
        Printf.sprintf "p%g" (lo_p *. 100.);
        "q1";
        "med";
        "q3";
        Printf.sprintf "p%g" (hi_p *. 100.);
        "max";
      ]
  in
  let make_row (label, samples) =
    let arr = Array.of_list samples in
    Array.sort Float.compare arr;
    let n = Array.length arr in
    let mean_val = Array.fold_left ( +. ) 0.0 arr /. float_of_int n in
    List.map B.text
      [
        label;
        string_of_int n;
        fmt mean_val;
        fmt (std_dev arr mean_val);
        fmt arr.(0);
        fmt (percentile arr lo_p);
        fmt (percentile arr 0.25);
        fmt (percentile arr 0.5);
        fmt (percentile arr 0.75);
        fmt (percentile arr hi_p);
        fmt arr.(n - 1);
      ]
  in
  B.frame @@ B.grid_l ~bars:true (header :: List.map make_row rows)

let with_title title b = B.vlist ~bars:false [ B.center_h @@ B.text title; b ]

(** Render a histogram panel for one stat. *)
let histogram_of_samples ?(bins = 10) ?(bar_width = 40) (label : string)
    (samples : float list) : B.t =
  let arr = Array.of_list samples in
  let n = Array.length arr in
  let lo = Array.fold_left Float.min Float.infinity arr in
  let hi = Array.fold_left Float.max Float.neg_infinity arr in
  let range = hi -. lo in
  let bin_width = if range = 0.0 then 1.0 else range /. float_of_int bins in
  let counts = Array.make bins 0 in
  Array.iter
    (fun v ->
      let b =
        if range = 0.0 then bins / 2
        else
          Int.min (bins - 1)
            (int_of_float ((v -. lo) /. range *. float_of_int bins))
      in
      counts.(b) <- counts.(b) + 1)
    arr;
  let max_count = Array.fold_left Int.max 0 counts in
  let rows =
    Array.to_list
      (Array.init bins (fun i ->
           let blo = lo +. (float_of_int i *. bin_width) in
           let bhi = blo +. bin_width in
           let c = counts.(i) in
           let bar_len =
             if max_count = 0 then 0 else c * bar_width / max_count
           in
           let bar = List.init bar_len (fun _ -> "█") |> String.concat "" in
           let pad = String.make (bar_width - bar_len) ' ' in
           let pct = float_of_int c /. float_of_int n *. 100.0 in
           B.hlist ~bars:false
             [
               B.text (Printf.sprintf "[%8.3g,%8.3g)" blo bhi);
               B.text (Printf.sprintf " %s%s %4d (%.1f%%)" bar pad c pct);
             ]))
  in
  B.frame (B.vlist ~bars:false (B.text label :: rows))

let sparkline_chars = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

let sparkline_of_samples ?(bins = 20) (samples : float list) : string =
  let arr = Array.of_list samples in
  let lo = Array.fold_left Float.min Float.infinity arr in
  let hi = Array.fold_left Float.max Float.neg_infinity arr in
  let range = hi -. lo in
  let counts = Array.make bins 0 in
  Array.iter
    (fun v ->
      let b =
        if range = 0.0 then bins / 2
        else
          Int.min (bins - 1)
            (int_of_float ((v -. lo) /. range *. float_of_int bins))
      in
      counts.(b) <- counts.(b) + 1)
    arr;
  let max_count = Array.fold_left Int.max 0 counts in
  Array.fold_left
    (fun acc c ->
      acc
      ^
      if c = 0 then " "
      else
        let level =
          Int.min 7
            (int_of_float (float_of_int c /. float_of_int max_count *. 8.0))
        in
        sparkline_chars.(level))
    "" counts

let sparklines_of_samples (stat_values : (string * float list) list) : B.t =
  let label_width = 20 in
  let rows =
    List.map
      (fun (label, values) ->
        let spark = sparkline_of_samples values in
        [| B.text (spf "%-*s" label_width label); B.text spark |])
      stat_values
  in
  B.frame @@ B.grid ~bars:true (Array.of_list rows)

let distr_of_gen ?(rand : Random.State.t = Random.State.make [| 0 |])
    ?(n = 1000) ?(width = 88) ?(max = true) ?(min = true) ?(mean = true)
    ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p
    ?(display : [ `Stat_table | `Boxplot | `Histogram | `Sparkline_hist ] =
      `Boxplot) ?(colors = false) (gen : 'a QCheck2.Gen.t)
    (stats : 'a stat list) : string =
  let samples = QCheck2.Gen.generate ~rand ~n gen in
  let (stat_values : (string * float list) list) =
    List.map (fun (label, f) -> (label, List.map f samples)) stats
  in
  let render : _ -> B.t = function
    | `Stat_table ->
        with_title "Stats Table" @@ table_of_samples ~lo_p ~hi_p stat_values
    | `Histogram ->
        with_title "Histogram"
        @@ B.vlist ~bars:false
             (List.map
                (fun (label, values) -> histogram_of_samples label values)
                stat_values)
    | `Boxplot ->
        with_title "Boxplot"
        @@ boxplot_of_samples ~width ~max ~min ~mean ~med ~lo_p ~hi_p ?q1_p
             ?q3_p n stat_values
    | `Sparkline_hist ->
        with_title "Histogram" @@ sparklines_of_samples stat_values
  in
  PrintBox_text.to_string_with ~style:colors @@ render display

let pp_gen ?(rand : Random.State.t = Random.State.make [| 0 |]) ?(n = 1000)
    ?(width = 88) ?(max = true) ?(min = true) ?(mean = true) ?(med = false)
    ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p
    ?(display : [ `Stat_table | `Boxplot | `Histogram | `Sparkline_hist ] =
      `Boxplot) ?(colors = false) () fmt (gen : 'a QCheck2.Gen.t)
    (stats : 'a QCheck2.stat list) : unit =
  let stats =
    List.map (fun (label, f) -> (label, Fun.compose float_of_int f)) stats
  in
  Format.fprintf fmt "%s"
    (distr_of_gen ~rand ~n ~width ~max ~min ~mean ~med ~lo_p ~hi_p ?q1_p ?q3_p
       ~display ~colors gen stats)

let%test_module _ =
  (module struct
    type tree = Leaf of int | Node of tree * tree

    let leaf x = Leaf x
    let node x y = Node (x, y)

    let (g : tree QCheck2.Gen.t) =
      QCheck2.Gen.(
        sized
        @@ fix (fun self n ->
            match n with
            | 0 -> map leaf nat
            | n ->
                oneof_weighted
                  [
                    (1, map leaf nat);
                    (2, map2 node (self (n / 2)) (self (n / 2)));
                  ]))

    let rec height = function
      | Leaf _ -> 0
      | Node (l, r) -> 1 + Int.max (height l) (height r)

    let rec size = function
      | Leaf _ -> 1
      | Node (l, r) -> 1 + size l + size r

    let leaf_count = size
    let internal_count t = size t - 1

    let leaf_depths t =
      let rec go depth = function
        | Leaf _ -> [ depth ]
        | Node (l, r) -> go (depth + 1) l @ go (depth + 1) r
      in
      go 0 t

    let min_depth t = leaf_depths t |> List.fold_left Int.min Int.max_int
    let max_depth t = height t

    let is_perfect t =
      let depths = leaf_depths t in
      List.for_all (fun d -> d = List.hd depths) depths

    let balance_factor t = max_depth t - min_depth t

    let level_widths t =
      let h = height t in
      let widths = Array.make (h + 1) 0 in
      let rec go depth = function
        | Leaf _ -> widths.(depth) <- widths.(depth) + 1
        | Node (l, r) ->
            widths.(depth) <- widths.(depth) + 1;
            go (depth + 1) l;
            go (depth + 1) r
      in
      go 0 t;
      Array.to_list widths

    let max_width t = level_widths t |> List.fold_left Int.max 0

    let rec left_spine = function
      | Leaf _ -> 0
      | Node (l, _) -> 1 + left_spine l

    let rec right_spine = function
      | Leaf _ -> 0
      | Node (_, r) -> 1 + right_spine r

    let rec is_left_skewed = function
      | Leaf _ -> true
      | Node (l, r) ->
          height l >= height r && is_left_skewed l && is_left_skewed r

    let total_path_length t = leaf_depths t |> List.fold_left ( + ) 0

    let avg_leaf_depth t =
      Float.of_int (total_path_length t) /. Float.of_int (leaf_count t)

    let tree_stats : tree stat list =
      let float_of_bool b = if b then 1.0 else -1.0 in
      [
        ("height", fun t -> height t |> float_of_int);
        ("leaf_count", fun t -> leaf_count t |> float_of_int);
        ("internal_count", fun t -> internal_count t |> float_of_int);
        ("min_depth", fun t -> min_depth t |> float_of_int);
        ("max_depth", fun t -> max_depth t |> float_of_int);
        ("is_perfect", fun t -> is_perfect t |> float_of_bool);
        ("balance_factor", fun t -> balance_factor t |> float_of_int);
        ("max_width", fun t -> max_width t |> float_of_int);
        ("left_spine", fun t -> left_spine t |> float_of_int);
        ("right_spine", fun t -> right_spine t |> float_of_int);
        ("is_left_skewed", fun t -> is_left_skewed t |> float_of_bool);
        ("avg_leaf_depth", fun t -> avg_leaf_depth t);
        ("total_path_length", fun t -> total_path_length t |> float_of_int);
      ]

    let%expect_test "boxplot" =
      let rand = Random.State.make [| 0 |] in
      let report = distr_of_gen ~rand ~n:1000 g tree_stats in
      print_endline report;
      [%expect
        {|
                                                      Boxplot
        ┌────────────────────────────┬────────────────────────────────────────────────────────────────────┐
        │n=1000                      │↓-1                                                              35↓│
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │height                      │ [-----+------------]                                               │
        │p5=0.00|p95=10.00|mu=2.90   │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │leaf_count                  │   [-----------------------------------------------+---------------~│
        │p5=1.00|p95=143.00|mu=26.51 │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │internal_count              │ [-----------------------------------------------+-----------------~│
        │p5=0.00|p95=142.00|mu=25.51 │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │min_depth                   │ [-+-]                                                              │
        │p5=0.00|p95=2.00|mu=0.94    │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │max_depth                   │ [-----+------------]                                               │
        │p5=0.00|p95=10.00|mu=2.90   │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │is_perfect                  │[+-]                                                                │
        │p5=-1.00|p95=1.00|mu=0.05   │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │balance_factor              │ [---+------------]                                                 │
        │p5=0.00|p95=9.00|mu=1.96    │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │max_width                   │   [------------+--------------------------------------------------~│
        │p5=1.00|p95=40.00|mu=8.08   │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │left_spine                  │ [--+------]                                                        │
        │p5=0.00|p95=5.00|mu=1.50    │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │right_spine                 │ [--+------]                                                        │
        │p5=0.00|p95=5.00|mu=1.53    │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │is_left_skewed              │[-+]                                                                │
        │p5=-1.00|p95=1.00|mu=0.24   │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │avg_leaf_depth              │ [--+----]                                                          │
        │p5=0.00|p95=4.31|mu=1.29    │                                                                    │
        ├────────────────────────────┼────────────────────────────────────────────────────────────────────┤
        │total_path_length           │ [-----------------------------------------------------------------~│
        │p5=0.00|p95=636.00|mu=102.72│                                                                    │
        └────────────────────────────┴────────────────────────────────────────────────────────────────────┘
        |}]

    let%expect_test "table" =
      let rand = Random.State.make [| 0 |] in
      let report = distr_of_gen ~rand ~n:1000 ~display:`Stat_table g tree_stats in
      print_endline report;
      [%expect
        {|
                                        Stats Table
        ┌─────────────────┬────┬─────┬──────┬────┬────┬────┬────┬─────┬─────┬─────┐
        │                 │n   │mean │std   │min │p5  │q1  │med │q3   │p95  │max  │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │height           │1000│2.898│3.38  │0   │0   │0   │2   │4    │10   │14   │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │leaf_count       │1000│26.51│65.14 │1   │1   │1   │5   │17   │143  │773  │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │internal_count   │1000│25.51│65.14 │0   │0   │0   │4   │16   │142  │772  │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │min_depth        │1000│0.935│0.8699│0   │0   │0   │1   │2    │2    │4    │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │max_depth        │1000│2.898│3.38  │0   │0   │0   │2   │4    │10   │14   │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │is_perfect       │1000│0.052│0.9986│-1  │-1  │-1  │1   │1    │1    │1    │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │balance_factor   │1000│1.963│2.9   │0   │0   │0   │0   │3    │9    │13   │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │max_width        │1000│8.083│17.55 │1   │1   │1   │2   │6    │40   │226  │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │left_spine       │1000│1.504│1.733 │0   │0   │0   │1   │2    │5    │12   │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │right_spine      │1000│1.529│1.74  │0   │0   │0   │1   │3    │5    │10   │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │is_left_skewed   │1000│0.242│0.9703│-1  │-1  │-1  │1   │1    │1    │1    │
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │avg_leaf_depth   │1000│1.293│1.415 │0   │0   │0   │1   │1.941│4.306│6.055│
        ├─────────────────┼────┼─────┼──────┼────┼────┼────┼────┼─────┼─────┼─────┤
        │total_path_length│1000│102.7│340.6 │0   │0   │0   │5   │35   │636  │4222 │
        └─────────────────┴────┴─────┴──────┴────┴────┴────┴────┴─────┴─────┴─────┘
        |}]

    let%expect_test "histogram" =
      let rand = Random.State.make [| 0 |] in
      let report =
        distr_of_gen ~rand ~n:1000 ~display:`Histogram g tree_stats
      in
      print_endline report;
      [%expect
        {|
                                         Histogram
        ┌─────────────────────────────────────────────────────────────────────────┐
        │height                                                                   │
        │[       0,     1.4) ████████████████████████████████████████  478 (47.8%)│
        │[     1.4,     2.8) ██████                                     83 (8.3%) │
        │[     2.8,     4.2) ████████████████                          202 (20.2%)│
        │[     4.2,     5.6) ██                                         25 (2.5%) │
        │[     5.6,       7) ████                                       51 (5.1%) │
        │[       7,     8.4) ████                                       54 (5.4%) │
        │[     8.4,     9.8) █                                          18 (1.8%) │
        │[     9.8,    11.2) █████                                      69 (6.9%) │
        │[    11.2,    12.6)                                             7 (0.7%) │
        │[    12.6,      14) █                                          13 (1.3%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │leaf_count                                                               │
        │[       1,    78.2) ████████████████████████████████████████  912 (91.2%)│
        │[    78.2,     155) █                                          45 (4.5%) │
        │[     155,     233) █                                          24 (2.4%) │
        │[     233,     310)                                             9 (0.9%) │
        │[     310,     387)                                             4 (0.4%) │
        │[     387,     464)                                             2 (0.2%) │
        │[     464,     541)                                             1 (0.1%) │
        │[     541,     619)                                             0 (0.0%) │
        │[     619,     696)                                             2 (0.2%) │
        │[     696,     773)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │internal_count                                                           │
        │[       0,    77.2) ████████████████████████████████████████  912 (91.2%)│
        │[    77.2,     154) █                                          45 (4.5%) │
        │[     154,     232) █                                          24 (2.4%) │
        │[     232,     309)                                             9 (0.9%) │
        │[     309,     386)                                             4 (0.4%) │
        │[     386,     463)                                             2 (0.2%) │
        │[     463,     540)                                             1 (0.1%) │
        │[     540,     618)                                             0 (0.0%) │
        │[     618,     695)                                             2 (0.2%) │
        │[     695,     772)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │min_depth                                                                │
        │[       0,     0.4) ███████████████████████████████████████   369 (36.9%)│
        │[     0.4,     0.8)                                             0 (0.0%) │
        │[     0.8,     1.2) ████████████████████████████████████████  370 (37.0%)│
        │[     1.2,     1.6)                                             0 (0.0%) │
        │[     1.6,       2)                                             0 (0.0%) │
        │[       2,     2.4) ███████████████████████                   219 (21.9%)│
        │[     2.4,     2.8)                                             0 (0.0%) │
        │[     2.8,     3.2) ████                                       41 (4.1%) │
        │[     3.2,     3.6)                                             0 (0.0%) │
        │[     3.6,       4)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │max_depth                                                                │
        │[       0,     1.4) ████████████████████████████████████████  478 (47.8%)│
        │[     1.4,     2.8) ██████                                     83 (8.3%) │
        │[     2.8,     4.2) ████████████████                          202 (20.2%)│
        │[     4.2,     5.6) ██                                         25 (2.5%) │
        │[     5.6,       7) ████                                       51 (5.1%) │
        │[       7,     8.4) ████                                       54 (5.4%) │
        │[     8.4,     9.8) █                                          18 (1.8%) │
        │[     9.8,    11.2) █████                                      69 (6.9%) │
        │[    11.2,    12.6)                                             7 (0.7%) │
        │[    12.6,      14) █                                          13 (1.3%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │is_perfect                                                               │
        │[      -1,    -0.8) ████████████████████████████████████      474 (47.4%)│
        │[    -0.8,    -0.6)                                             0 (0.0%) │
        │[    -0.6,    -0.4)                                             0 (0.0%) │
        │[    -0.4,    -0.2)                                             0 (0.0%) │
        │[    -0.2,5.55e-17)                                             0 (0.0%) │
        │[5.55e-17,     0.2)                                             0 (0.0%) │
        │[     0.2,     0.4)                                             0 (0.0%) │
        │[     0.4,     0.6)                                             0 (0.0%) │
        │[     0.6,     0.8)                                             0 (0.0%) │
        │[     0.8,       1) ████████████████████████████████████████  526 (52.6%)│
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │balance_factor                                                           │
        │[       0,     1.3) ████████████████████████████████████████  622 (62.2%)│
        │[     1.3,     2.6) ██████                                    103 (10.3%)│
        │[     2.6,     3.9) ███                                        55 (5.5%) │
        │[     3.9,     5.2) █████                                      81 (8.1%) │
        │[     5.2,     6.5) █                                          30 (3.0%) │
        │[     6.5,     7.8) █                                          19 (1.9%) │
        │[     7.8,     9.1) ████                                       69 (6.9%) │
        │[     9.1,    10.4)                                             6 (0.6%) │
        │[    10.4,    11.7)                                             7 (0.7%) │
        │[    11.7,      13)                                             8 (0.8%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │max_width                                                                │
        │[       1,    23.5) ████████████████████████████████████████  914 (91.4%)│
        │[    23.5,      46) ██                                         48 (4.8%) │
        │[      46,    68.5) █                                          25 (2.5%) │
        │[    68.5,      91)                                             3 (0.3%) │
        │[      91,     114)                                             4 (0.4%) │
        │[     114,     136)                                             2 (0.2%) │
        │[     136,     158)                                             2 (0.2%) │
        │[     158,     181)                                             1 (0.1%) │
        │[     181,     204)                                             0 (0.0%) │
        │[     204,     226)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │left_spine                                                               │
        │[       0,     1.2) ████████████████████████████████████████  612 (61.2%)│
        │[     1.2,     2.4) █████████                                 146 (14.6%)│
        │[     2.4,     3.6) ████████                                  129 (12.9%)│
        │[     3.6,     4.8) ███                                        58 (5.8%) │
        │[     4.8,       6) █                                          20 (2.0%) │
        │[       6,     7.2) █                                          26 (2.6%) │
        │[     7.2,     8.4)                                             4 (0.4%) │
        │[     8.4,     9.6)                                             3 (0.3%) │
        │[     9.6,    10.8)                                             1 (0.1%) │
        │[    10.8,      12)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │right_spine                                                              │
        │[       0,       1) ████████████████████████████████████████  369 (36.9%)│
        │[       1,       2) █████████████████████████                 236 (23.6%)│
        │[       2,       3) ███████████████                           144 (14.4%)│
        │[       3,       4) █████████████                             125 (12.5%)│
        │[       4,       5) ███████                                    69 (6.9%) │
        │[       5,       6) ██                                         23 (2.3%) │
        │[       6,       7) █                                          18 (1.8%) │
        │[       7,       8)                                             9 (0.9%) │
        │[       8,       9)                                             0 (0.0%) │
        │[       9,      10)                                             7 (0.7%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │is_left_skewed                                                           │
        │[      -1,    -0.8) ████████████████████████                  379 (37.9%)│
        │[    -0.8,    -0.6)                                             0 (0.0%) │
        │[    -0.6,    -0.4)                                             0 (0.0%) │
        │[    -0.4,    -0.2)                                             0 (0.0%) │
        │[    -0.2,5.55e-17)                                             0 (0.0%) │
        │[5.55e-17,     0.2)                                             0 (0.0%) │
        │[     0.2,     0.4)                                             0 (0.0%) │
        │[     0.4,     0.6)                                             0 (0.0%) │
        │[     0.6,     0.8)                                             0 (0.0%) │
        │[     0.8,       1) ████████████████████████████████████████  621 (62.1%)│
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │avg_leaf_depth                                                           │
        │[       0,   0.605) ████████████████████████████████████████  369 (36.9%)│
        │[   0.605,    1.21) ████████████████████                      192 (19.2%)│
        │[    1.21,    1.82) ████████████████                          153 (15.3%)│
        │[    1.82,    2.42) █████████                                  88 (8.8%) │
        │[    2.42,    3.03) ██████                                     59 (5.9%) │
        │[    3.03,    3.63) ████                                       43 (4.3%) │
        │[    3.63,    4.24) ███                                        34 (3.4%) │
        │[    4.24,    4.84) ████                                       43 (4.3%) │
        │[    4.84,    5.45)                                             7 (0.7%) │
        │[    5.45,    6.05) █                                          12 (1.2%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │total_path_length                                                        │
        │[       0,     422) ████████████████████████████████████████  931 (93.1%)│
        │[     422,     844) █                                          40 (4.0%) │
        │[     844,1.27e+03)                                            16 (1.6%) │
        │[1.27e+03,1.69e+03)                                             4 (0.4%) │
        │[1.69e+03,2.11e+03)                                             2 (0.2%) │
        │[2.11e+03,2.53e+03)                                             3 (0.3%) │
        │[2.53e+03,2.96e+03)                                             1 (0.1%) │
        │[2.96e+03,3.38e+03)                                             0 (0.0%) │
        │[3.38e+03, 3.8e+03)                                             1 (0.1%) │
        │[ 3.8e+03,4.22e+03)                                             2 (0.2%) │
        └─────────────────────────────────────────────────────────────────────────┘
        |}]

    let%expect_test "sparkline" =
      let rand = Random.State.make [| 0 |] in
      let report =
        distr_of_gen ~rand ~n:1000 ~display:`Sparkline_hist g tree_stats
      in
      print_endline report;
      [%expect
        {|
                         Histogram
        ┌────────────────────┬────────────────────┐
        │height              │█▃▂ ▃▂ ▁▂ ▁▁▁ ▂▁ ▁▁▁│
        ├────────────────────┼────────────────────┤
        │leaf_count          │█▁▁▁▁▁▁▁▁▁▁ ▁   ▁▁ ▁│
        ├────────────────────┼────────────────────┤
        │internal_count      │█▁▁▁▁▁▁▁▁▁▁ ▁   ▁▁ ▁│
        ├────────────────────┼────────────────────┤
        │min_depth           │█    █    ▅    ▁   ▁│
        ├────────────────────┼────────────────────┤
        │max_depth           │█▃▂ ▃▂ ▁▂ ▁▁▁ ▂▁ ▁▁▁│
        ├────────────────────┼────────────────────┤
        │is_perfect          │█                  █│
        ├────────────────────┼────────────────────┤
        │balance_factor      │█▂ ▂▁ ▁▁ ▁▁ ▁▁ ▁▁ ▁▁│
        ├────────────────────┼────────────────────┤
        │max_width           │█▁▁▁▁▁▁▁▁ ▁▁▁▁▁    ▁│
        ├────────────────────┼────────────────────┤
        │left_spine          │█▆ ▄ ▃▂ ▁ ▁▁ ▁ ▁▁  ▁│
        ├────────────────────┼────────────────────┤
        │right_spine         │█ ▆ ▄ ▃ ▂ ▁ ▁ ▁   ▁▁│
        ├────────────────────┼────────────────────┤
        │is_left_skewed      │▅                  █│
        ├────────────────────┼────────────────────┤
        │avg_leaf_depth      │█ ▃▂▂▂▂▁▁▁▁▁▁▁▁▁▁▁▁▁│
        ├────────────────────┼────────────────────┤
        │total_path_length   │█▁▁▁▁▁▁▁▁ ▁▁ ▁   ▁▁▁│
        └────────────────────┴────────────────────┘
        |}]
  end)
