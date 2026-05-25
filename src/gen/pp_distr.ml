(** {0:distr Visualise QCheck2 generator distributions} *)

module B = PrintBox

let spf = Printf.sprintf

type 'a stat = string * ('a -> float)
type display = Table | Boxplot | Histogram

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
let axis_header ~(width : int) ~(n : int) ~(scale_lo : float)
    ~(scale_hi : float) : B.t =
  let lo_str = Printf.sprintf "%.4g" scale_lo in
  let hi_str = Printf.sprintf "%.4g" scale_hi in
  let gap = width - String.length lo_str - String.length hi_str in
  let axis_str = spf "|%s%s%s|" lo_str (String.make gap ' ') hi_str in
  let pad20 s = spf "%-20s" s in
  B.(
    hlist ~bars:false [ text (pad20 (spf "n=%d" n)); text axis_str ])

(** Draw a single box-plot row as a string of [width] chars, given integer
    positions already mapped to [0..width-1]. [mean] is marked with [+], [med]
    with [|]; both are optional. *)
let render_boxplot ~width ~lo ~hi ?mean ?med ?q1 ?q3 () =
  let line = Bytes.make width ' ' in
  Bytes.fill line (lo + 1) (hi - lo - 1) '-';
  Bytes.set line lo '[';
  Bytes.set line hi ']';
  (match q1 with
  | Some m -> Bytes.set line m '<'
  | None -> ());
  (match q3 with
  | Some m -> Bytes.set line m '>'
  | None -> ());
  (match mean with
  | Some m -> Bytes.set line m '+'
  | None -> ());
  (match med with
  | Some m -> Bytes.set line m '*'
  | None -> ());
  Bytes.to_string line

(** Render one boxplot row. [scale_lo]/[scale_hi] override the axis endpoints
    for column mapping (universal scale across multiple stats); when absent the
    per-stat [lo_p]/[hi_p] percentiles define the axis. *)
let boxplot_of_samples ?(width = 88) ?(max = true) ?(min = true) ?(mean = true)
    ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p ?scale_lo ?scale_hi
    (label : string) (samples : float list) : B.t =
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
  let range = shi -. slo in
  let col v =
    if range = 0.0 then width / 2
    else
      Int.min (width - 1)
        (int_of_float ((v -. slo) *. float_of_int (width - 1) /. range))
  in
  let plot =
    render_boxplot ~width ~lo:(col lo) ~hi:(col hi) ?q1:(Option.map col q1)
      ~mean:(col mean_val)
      ?med:(if med then Some (col median) else None)
      ?q3:(Option.map col q3) ()
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
    " └" ^ String.concat "|" parts
  in
  B.(
    vlist ~bars:false
      [
        hlist ~bars:false [ text (Printf.sprintf "%-20s" label); text plot ];
        text stats;
      ])

(** Render all stats as a describe()-style table (n, mean, std, min, pLO, q1,
    med, q3, pHI, max). *)
let table_of_samples ?(lo_p = 0.05) ?(hi_p = 0.95)
    (rows : (string * float list) list) : B.t =
  let fmt v = Printf.sprintf "%.4g" v in
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
  B.grid_l ~bars:true (header :: List.map make_row rows)

(** Render a histogram panel for one stat. *)
let histogram_of_samples ?(bins = 20) ?(bar_width = 40) (label : string)
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
           let bar = String.make bar_len '#' in
           let pct = float_of_int c /. float_of_int n *. 100.0 in
           B.hlist ~bars:false
             [
               B.text (Printf.sprintf "[%8.3g,%8.3g)" blo bhi);
               B.text (Printf.sprintf " %-*s %4d (%.1f%%)" bar_width bar c pct);
             ]))
  in
  B.frame (B.vlist ~bars:false (B.text label :: rows))

let pp_gen_distr ?(rand : Random.State.t = Random.State.make [| 0 |])
    ?(n = 1000) ?(width = 88) ?(max = true) ?(min = true) ?(mean = true)
    ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p
    ?(display = [ Boxplot ]) ?(colors = false) (gen : 'a QCheck2.Gen.t)
    (stats : 'a stat list) : string =
  let samples = QCheck2.Gen.generate ~rand ~n gen in
  let stat_values =
    List.map (fun (label, f) -> (label, List.map f samples)) stats
  in
  let render_section = function
    | Table -> table_of_samples ~lo_p ~hi_p stat_values
    | Histogram ->
        B.vlist ~bars:false
          (List.map
             (fun (label, values) -> histogram_of_samples label values)
             stat_values)
    | Boxplot ->
        let all_arr = Array.of_list (List.concat_map snd stat_values) in
        Array.sort Float.compare all_arr;
        let scale_lo = percentile all_arr lo_p in
        let scale_hi = percentile all_arr hi_p in
        let header = axis_header ~width ~n ~scale_lo ~scale_hi in
        let boxes =
          List.map
            (fun (label, values) ->
              boxplot_of_samples ~width ~max ~min ~mean ~med ~lo_p ~hi_p ?q1_p
                ?q3_p ~scale_lo ~scale_hi label values)
            stat_values
        in
        B.(frame (vlist ~bars:false (header :: boxes)))
  in
  let sections = List.map render_section display in
  let report = B.(vlist ~bars:false sections) in
  PrintBox_text.to_string_with ~style:colors report

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
      let report = pp_gen_distr ~rand ~n:1000 g tree_stats in
      print_endline report;
      [%expect
        {|
        ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
        │n=1000              |-1                                                                                    35|│
        │height                [------+----------------]                                                               │
        │ └p5=0.00|p95=10.00|mu=2.90                                                                                   │
        │leaf_count              [-------------------------------------------------------------+--------------------]  │
        │ └p5=1.00|p95=143.00|mu=26.51                                                                                 │
        │internal_count        [-------------------------------------------------------------+----------------------]  │
        │ └p5=0.00|p95=142.00|mu=25.51                                                                                 │
        │min_depth             [-+--]                                                                                  │
        │ └p5=0.00|p95=2.00|mu=0.94                                                                                    │
        │max_depth             [------+----------------]                                                               │
        │ └p5=0.00|p95=10.00|mu=2.90                                                                                   │
        │is_perfect          [-+-]                                                                                     │
        │ └p5=-1.00|p95=1.00|mu=0.05                                                                                   │
        │balance_factor        [----+----------------]                                                                 │
        │ └p5=0.00|p95=9.00|mu=1.96                                                                                    │
        │max_width               [----------------+-----------------------------------------------------------------]  │
        │ └p5=1.00|p95=40.00|mu=8.08                                                                                   │
        │left_spine            [---+-------]                                                                           │
        │ └p5=0.00|p95=5.00|mu=1.50                                                                                    │
        │right_spine           [---+-------]                                                                           │
        │ └p5=0.00|p95=5.00|mu=1.53                                                                                    │
        │is_left_skewed      [--+]                                                                                     │
        │ └p5=-1.00|p95=1.00|mu=0.24                                                                                   │
        │avg_leaf_depth        [--+------]                                                                             │
        │ └p5=0.00|p95=4.31|mu=1.29                                                                                    │
        │total_path_length     [------------------------------------------------------------------------------------+  │
        │ └p5=0.00|p95=636.00|mu=102.72                                                                                │
        └──────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
        |}]

    let%expect_test "table" =
      let rand = Random.State.make [| 0 |] in
      let report = pp_gen_distr ~rand ~n:1000 ~display:[ Table ] g tree_stats in
      print_endline report;
      [%expect
        {|
                         │n   │mean │std   │min│p5│q1│med│q3   │p95  │max
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        height           │1000│2.898│3.38  │0  │0 │0 │2  │4    │10   │14
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        leaf_count       │1000│26.51│65.14 │1  │1 │1 │5  │17   │143  │773
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        internal_count   │1000│25.51│65.14 │0  │0 │0 │4  │16   │142  │772
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        min_depth        │1000│0.935│0.8699│0  │0 │0 │1  │2    │2    │4
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        max_depth        │1000│2.898│3.38  │0  │0 │0 │2  │4    │10   │14
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        is_perfect       │1000│0.052│0.9986│-1 │-1│-1│1  │1    │1    │1
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        balance_factor   │1000│1.963│2.9   │0  │0 │0 │0  │3    │9    │13
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        max_width        │1000│8.083│17.55 │1  │1 │1 │2  │6    │40   │226
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        left_spine       │1000│1.504│1.733 │0  │0 │0 │1  │2    │5    │12
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        right_spine      │1000│1.529│1.74  │0  │0 │0 │1  │3    │5    │10
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        is_left_skewed   │1000│0.242│0.9703│-1 │-1│-1│1  │1    │1    │1
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        avg_leaf_depth   │1000│1.293│1.415 │0  │0 │0 │1  │1.941│4.306│6.055
        ─────────────────┼────┼─────┼──────┼───┼──┼──┼───┼─────┼─────┼─────
        total_path_length│1000│102.7│340.6 │0  │0 │0 │5  │35   │636  │4222
        |}]

    let%expect_test "histogram" =
      let rand = Random.State.make [| 0 |] in
      let report =
        pp_gen_distr ~rand ~n:1000 ~display:[ Histogram ] g tree_stats
      in
      print_endline report;
      [%expect
        {|
        ┌─────────────────────────────────────────────────────────────────────────┐
        │height                                                                   │
        │[       0,     0.7) ########################################  369 (36.9%)│
        │[     0.7,     1.4) ###########                               109 (10.9%)│
        │[     1.4,     2.1) ########                                   83 (8.3%) │
        │[     2.1,     2.8)                                             0 (0.0%) │
        │[     2.8,     3.5) ############                              115 (11.5%)│
        │[     3.5,     4.2) #########                                  87 (8.7%) │
        │[     4.2,     4.9)                                             0 (0.0%) │
        │[     4.9,     5.6) ##                                         25 (2.5%) │
        │[     5.6,     6.3) #####                                      51 (5.1%) │
        │[     6.3,       7)                                             0 (0.0%) │
        │[       7,     7.7) ####                                       43 (4.3%) │
        │[     7.7,     8.4) #                                          11 (1.1%) │
        │[     8.4,     9.1) #                                          18 (1.8%) │
        │[     9.1,     9.8)                                             0 (0.0%) │
        │[     9.8,    10.5) #######                                    67 (6.7%) │
        │[    10.5,    11.2)                                             2 (0.2%) │
        │[    11.2,    11.9)                                             0 (0.0%) │
        │[    11.9,    12.6)                                             7 (0.7%) │
        │[    12.6,    13.3) #                                          10 (1.0%) │
        │[    13.3,      14)                                             3 (0.3%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │leaf_count                                                               │
        │[       1,    39.6) ########################################  839 (83.9%)│
        │[    39.6,    78.2) ###                                        73 (7.3%) │
        │[    78.2,     117) #                                          21 (2.1%) │
        │[     117,     155) #                                          24 (2.4%) │
        │[     155,     194)                                            16 (1.6%) │
        │[     194,     233)                                             8 (0.8%) │
        │[     233,     271)                                             6 (0.6%) │
        │[     271,     310)                                             3 (0.3%) │
        │[     310,     348)                                             2 (0.2%) │
        │[     348,     387)                                             2 (0.2%) │
        │[     387,     426)                                             2 (0.2%) │
        │[     426,     464)                                             0 (0.0%) │
        │[     464,     503)                                             1 (0.1%) │
        │[     503,     541)                                             0 (0.0%) │
        │[     541,     580)                                             0 (0.0%) │
        │[     580,     619)                                             0 (0.0%) │
        │[     619,     657)                                             1 (0.1%) │
        │[     657,     696)                                             1 (0.1%) │
        │[     696,     734)                                             0 (0.0%) │
        │[     734,     773)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │internal_count                                                           │
        │[       0,    38.6) ########################################  839 (83.9%)│
        │[    38.6,    77.2) ###                                        73 (7.3%) │
        │[    77.2,     116) #                                          21 (2.1%) │
        │[     116,     154) #                                          24 (2.4%) │
        │[     154,     193)                                            16 (1.6%) │
        │[     193,     232)                                             8 (0.8%) │
        │[     232,     270)                                             6 (0.6%) │
        │[     270,     309)                                             3 (0.3%) │
        │[     309,     347)                                             2 (0.2%) │
        │[     347,     386)                                             2 (0.2%) │
        │[     386,     425)                                             2 (0.2%) │
        │[     425,     463)                                             0 (0.0%) │
        │[     463,     502)                                             1 (0.1%) │
        │[     502,     540)                                             0 (0.0%) │
        │[     540,     579)                                             0 (0.0%) │
        │[     579,     618)                                             0 (0.0%) │
        │[     618,     656)                                             1 (0.1%) │
        │[     656,     695)                                             1 (0.1%) │
        │[     695,     733)                                             0 (0.0%) │
        │[     733,     772)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │min_depth                                                                │
        │[       0,     0.2) #######################################   369 (36.9%)│
        │[     0.2,     0.4)                                             0 (0.0%) │
        │[     0.4,     0.6)                                             0 (0.0%) │
        │[     0.6,     0.8)                                             0 (0.0%) │
        │[     0.8,       1)                                             0 (0.0%) │
        │[       1,     1.2) ########################################  370 (37.0%)│
        │[     1.2,     1.4)                                             0 (0.0%) │
        │[     1.4,     1.6)                                             0 (0.0%) │
        │[     1.6,     1.8)                                             0 (0.0%) │
        │[     1.8,       2)                                             0 (0.0%) │
        │[       2,     2.2) #######################                   219 (21.9%)│
        │[     2.2,     2.4)                                             0 (0.0%) │
        │[     2.4,     2.6)                                             0 (0.0%) │
        │[     2.6,     2.8)                                             0 (0.0%) │
        │[     2.8,       3)                                             0 (0.0%) │
        │[       3,     3.2) ####                                       41 (4.1%) │
        │[     3.2,     3.4)                                             0 (0.0%) │
        │[     3.4,     3.6)                                             0 (0.0%) │
        │[     3.6,     3.8)                                             0 (0.0%) │
        │[     3.8,       4)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │max_depth                                                                │
        │[       0,     0.7) ########################################  369 (36.9%)│
        │[     0.7,     1.4) ###########                               109 (10.9%)│
        │[     1.4,     2.1) ########                                   83 (8.3%) │
        │[     2.1,     2.8)                                             0 (0.0%) │
        │[     2.8,     3.5) ############                              115 (11.5%)│
        │[     3.5,     4.2) #########                                  87 (8.7%) │
        │[     4.2,     4.9)                                             0 (0.0%) │
        │[     4.9,     5.6) ##                                         25 (2.5%) │
        │[     5.6,     6.3) #####                                      51 (5.1%) │
        │[     6.3,       7)                                             0 (0.0%) │
        │[       7,     7.7) ####                                       43 (4.3%) │
        │[     7.7,     8.4) #                                          11 (1.1%) │
        │[     8.4,     9.1) #                                          18 (1.8%) │
        │[     9.1,     9.8)                                             0 (0.0%) │
        │[     9.8,    10.5) #######                                    67 (6.7%) │
        │[    10.5,    11.2)                                             2 (0.2%) │
        │[    11.2,    11.9)                                             0 (0.0%) │
        │[    11.9,    12.6)                                             7 (0.7%) │
        │[    12.6,    13.3) #                                          10 (1.0%) │
        │[    13.3,      14)                                             3 (0.3%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │is_perfect                                                               │
        │[      -1,    -0.9) ####################################      474 (47.4%)│
        │[    -0.9,    -0.8)                                             0 (0.0%) │
        │[    -0.8,    -0.7)                                             0 (0.0%) │
        │[    -0.7,    -0.6)                                             0 (0.0%) │
        │[    -0.6,    -0.5)                                             0 (0.0%) │
        │[    -0.5,    -0.4)                                             0 (0.0%) │
        │[    -0.4,    -0.3)                                             0 (0.0%) │
        │[    -0.3,    -0.2)                                             0 (0.0%) │
        │[    -0.2,    -0.1)                                             0 (0.0%) │
        │[    -0.1,5.55e-17)                                             0 (0.0%) │
        │[5.55e-17,     0.1)                                             0 (0.0%) │
        │[     0.1,     0.2)                                             0 (0.0%) │
        │[     0.2,     0.3)                                             0 (0.0%) │
        │[     0.3,     0.4)                                             0 (0.0%) │
        │[     0.4,     0.5)                                             0 (0.0%) │
        │[     0.5,     0.6)                                             0 (0.0%) │
        │[     0.6,     0.7)                                             0 (0.0%) │
        │[     0.7,     0.8)                                             0 (0.0%) │
        │[     0.8,     0.9)                                             0 (0.0%) │
        │[     0.9,       1) ########################################  526 (52.6%)│
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │balance_factor                                                           │
        │[       0,    0.65) ########################################  526 (52.6%)│
        │[    0.65,     1.3) #######                                    96 (9.6%) │
        │[     1.3,    1.95)                                             0 (0.0%) │
        │[    1.95,     2.6) #######                                   103 (10.3%)│
        │[     2.6,    3.25) ####                                       55 (5.5%) │
        │[    3.25,     3.9)                                             0 (0.0%) │
        │[     3.9,    4.55) ###                                        40 (4.0%) │
        │[    4.55,     5.2) ###                                        41 (4.1%) │
        │[     5.2,    5.85)                                             0 (0.0%) │
        │[    5.85,     6.5) ##                                         30 (3.0%) │
        │[     6.5,    7.15) #                                          19 (1.9%) │
        │[    7.15,     7.8)                                             0 (0.0%) │
        │[     7.8,    8.45) ##                                         37 (3.7%) │
        │[    8.45,     9.1) ##                                         32 (3.2%) │
        │[     9.1,    9.75)                                             0 (0.0%) │
        │[    9.75,    10.4)                                             6 (0.6%) │
        │[    10.4,    11.1)                                             7 (0.7%) │
        │[    11.1,    11.7)                                             0 (0.0%) │
        │[    11.7,    12.4)                                             7 (0.7%) │
        │[    12.3,      13)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │max_width                                                                │
        │[       1,    12.2) ########################################  856 (85.6%)│
        │[    12.2,    23.5) ##                                         58 (5.8%) │
        │[    23.5,    34.8) #                                          30 (3.0%) │
        │[    34.8,      46)                                            18 (1.8%) │
        │[      46,    57.2)                                            12 (1.2%) │
        │[    57.2,    68.5)                                            13 (1.3%) │
        │[    68.5,    79.8)                                             2 (0.2%) │
        │[    79.8,      91)                                             1 (0.1%) │
        │[      91,     102)                                             4 (0.4%) │
        │[     102,     114)                                             0 (0.0%) │
        │[     114,     125)                                             1 (0.1%) │
        │[     125,     136)                                             1 (0.1%) │
        │[     136,     147)                                             1 (0.1%) │
        │[     147,     158)                                             1 (0.1%) │
        │[     158,     170)                                             1 (0.1%) │
        │[     170,     181)                                             0 (0.0%) │
        │[     181,     192)                                             0 (0.0%) │
        │[     192,     204)                                             0 (0.0%) │
        │[     204,     215)                                             0 (0.0%) │
        │[     215,     226)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │left_spine                                                               │
        │[       0,     0.6) ########################################  369 (36.9%)│
        │[     0.6,     1.2) ##########################                243 (24.3%)│
        │[     1.2,     1.8)                                             0 (0.0%) │
        │[     1.8,     2.4) ###############                           146 (14.6%)│
        │[     2.4,       3)                                             0 (0.0%) │
        │[       3,     3.6) #############                             129 (12.9%)│
        │[     3.6,     4.2) ######                                     58 (5.8%) │
        │[     4.2,     4.8)                                             0 (0.0%) │
        │[     4.8,     5.4) ##                                         20 (2.0%) │
        │[     5.4,       6)                                             0 (0.0%) │
        │[       6,     6.6) #                                          13 (1.3%) │
        │[     6.6,     7.2) #                                          13 (1.3%) │
        │[     7.2,     7.8)                                             0 (0.0%) │
        │[     7.8,     8.4)                                             4 (0.4%) │
        │[     8.4,       9)                                             0 (0.0%) │
        │[       9,     9.6)                                             3 (0.3%) │
        │[     9.6,    10.2)                                             1 (0.1%) │
        │[    10.2,    10.8)                                             0 (0.0%) │
        │[    10.8,    11.4)                                             0 (0.0%) │
        │[    11.4,      12)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │right_spine                                                              │
        │[       0,     0.5) ########################################  369 (36.9%)│
        │[     0.5,       1)                                             0 (0.0%) │
        │[       1,     1.5) #########################                 236 (23.6%)│
        │[     1.5,       2)                                             0 (0.0%) │
        │[       2,     2.5) ###############                           144 (14.4%)│
        │[     2.5,       3)                                             0 (0.0%) │
        │[       3,     3.5) #############                             125 (12.5%)│
        │[     3.5,       4)                                             0 (0.0%) │
        │[       4,     4.5) #######                                    69 (6.9%) │
        │[     4.5,       5)                                             0 (0.0%) │
        │[       5,     5.5) ##                                         23 (2.3%) │
        │[     5.5,       6)                                             0 (0.0%) │
        │[       6,     6.5) #                                          18 (1.8%) │
        │[     6.5,       7)                                             0 (0.0%) │
        │[       7,     7.5)                                             9 (0.9%) │
        │[     7.5,       8)                                             0 (0.0%) │
        │[       8,     8.5)                                             0 (0.0%) │
        │[     8.5,       9)                                             0 (0.0%) │
        │[       9,     9.5)                                             2 (0.2%) │
        │[     9.5,      10)                                             5 (0.5%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │is_left_skewed                                                           │
        │[      -1,    -0.9) ########################                  379 (37.9%)│
        │[    -0.9,    -0.8)                                             0 (0.0%) │
        │[    -0.8,    -0.7)                                             0 (0.0%) │
        │[    -0.7,    -0.6)                                             0 (0.0%) │
        │[    -0.6,    -0.5)                                             0 (0.0%) │
        │[    -0.5,    -0.4)                                             0 (0.0%) │
        │[    -0.4,    -0.3)                                             0 (0.0%) │
        │[    -0.3,    -0.2)                                             0 (0.0%) │
        │[    -0.2,    -0.1)                                             0 (0.0%) │
        │[    -0.1,5.55e-17)                                             0 (0.0%) │
        │[5.55e-17,     0.1)                                             0 (0.0%) │
        │[     0.1,     0.2)                                             0 (0.0%) │
        │[     0.2,     0.3)                                             0 (0.0%) │
        │[     0.3,     0.4)                                             0 (0.0%) │
        │[     0.4,     0.5)                                             0 (0.0%) │
        │[     0.5,     0.6)                                             0 (0.0%) │
        │[     0.6,     0.7)                                             0 (0.0%) │
        │[     0.7,     0.8)                                             0 (0.0%) │
        │[     0.8,     0.9)                                             0 (0.0%) │
        │[     0.9,       1) ########################################  621 (62.1%)│
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │avg_leaf_depth                                                           │
        │[       0,   0.303) ########################################  369 (36.9%)│
        │[   0.303,   0.605)                                             0 (0.0%) │
        │[   0.605,   0.908) ###########                               109 (10.9%)│
        │[   0.908,    1.21) ########                                   83 (8.3%) │
        │[    1.21,    1.51) #########                                  85 (8.5%) │
        │[    1.51,    1.82) #######                                    68 (6.8%) │
        │[    1.82,    2.12) ######                                     63 (6.3%) │
        │[    2.12,    2.42) ##                                         25 (2.5%) │
        │[    2.42,    2.72) ####                                       40 (4.0%) │
        │[    2.72,    3.03) ##                                         19 (1.9%) │
        │[    3.03,    3.33) ###                                        33 (3.3%) │
        │[    3.33,    3.63) #                                          10 (1.0%) │
        │[    3.63,    3.94) #                                          15 (1.5%) │
        │[    3.94,    4.24) ##                                         19 (1.9%) │
        │[    4.24,    4.54) ####                                       40 (4.0%) │
        │[    4.54,    4.84)                                             3 (0.3%) │
        │[    4.84,    5.15)                                             1 (0.1%) │
        │[    5.15,    5.45)                                             6 (0.6%) │
        │[    5.45,    5.75)                                             7 (0.7%) │
        │[    5.75,    6.05)                                             5 (0.5%) │
        └─────────────────────────────────────────────────────────────────────────┘
        ┌─────────────────────────────────────────────────────────────────────────┐
        │total_path_length                                                        │
        │[       0,     211) ########################################  895 (89.5%)│
        │[     211,     422) #                                          36 (3.6%) │
        │[     422,     633)                                            19 (1.9%) │
        │[     633,     844)                                            21 (2.1%) │
        │[     844,1.06e+03)                                             9 (0.9%) │
        │[1.06e+03,1.27e+03)                                             7 (0.7%) │
        │[1.27e+03,1.48e+03)                                             2 (0.2%) │
        │[1.48e+03,1.69e+03)                                             2 (0.2%) │
        │[1.69e+03, 1.9e+03)                                             2 (0.2%) │
        │[ 1.9e+03,2.11e+03)                                             0 (0.0%) │
        │[2.11e+03,2.32e+03)                                             1 (0.1%) │
        │[2.32e+03,2.53e+03)                                             2 (0.2%) │
        │[2.53e+03,2.74e+03)                                             0 (0.0%) │
        │[2.74e+03,2.96e+03)                                             1 (0.1%) │
        │[2.96e+03,3.17e+03)                                             0 (0.0%) │
        │[3.17e+03,3.38e+03)                                             0 (0.0%) │
        │[3.38e+03,3.59e+03)                                             0 (0.0%) │
        │[3.59e+03, 3.8e+03)                                             1 (0.1%) │
        │[ 3.8e+03,4.01e+03)                                             1 (0.1%) │
        │[4.01e+03,4.22e+03)                                             1 (0.1%) │
        └─────────────────────────────────────────────────────────────────────────┘
        |}]
  end)
