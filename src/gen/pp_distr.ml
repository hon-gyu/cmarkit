(** {0:distr Visualise QCheck2 generator distributions} *)

module B = PrintBox

type 'a stat = string * ('a -> float)

let percentile (sorted : float array) (p : float) : float =
  let n = Array.length sorted in
  let i = int_of_float (float_of_int n *. p) in
  sorted.(Int.min i (n - 1))

(** Draw a single box-plot row as a string of [width] chars, given integer
    positions already mapped to [0..width]. *)
let render_boxplot ~width ~lo ~hi ?q1 ?med ?q3 () =
  let line = Bytes.make width '-' in
  (match (q1, q3) with
  | Some q1, Some q3 ->
      for i = q1 to q3 do
        Bytes.set line i ' '
      done;
      Bytes.set line q1 '[';
      Bytes.set line q3 ']'
  | _ -> ());
  Bytes.set line lo '|';
  Bytes.set line hi '|';
  (match med with
  | Some m -> Bytes.set line m '|'
  | None -> ());
  Bytes.to_string line

let boxplot_of_samples ?(width = 88) ?(max = true) ?(min = true) ?(mean = true)
    ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p (label : string)
    (samples : float list) : B.t =
  let arr = Array.of_list samples in
  Array.sort Float.compare arr;
  let n = Array.length arr in
  let lo = percentile arr lo_p in
  let hi = percentile arr hi_p in
  let median = percentile arr 0.5 in
  let mean_val = Array.fold_left ( +. ) 0.0 arr /. float_of_int n in
  let q1 = Option.map (percentile arr) q1_p in
  let q3 = Option.map (percentile arr) q3_p in
  let range = hi -. lo in
  let col v =
    if range = 0.0 then width / 2
    else
      Int.min (width - 1)
        (int_of_float ((v -. lo) *. float_of_int (width - 1) /. range))
  in
  let plot =
    render_boxplot ~width ~lo:(col lo) ~hi:(col hi) ?q1:(Option.map col q1)
      ?med:(if med then Some (col median) else None)
      ?q3:(Option.map col q3) ()
  in
  let stats =
    let buf = Buffer.create 64 in
    Buffer.add_string buf (Printf.sprintf "n=%d" n);
    if min then Buffer.add_string buf (Printf.sprintf "  lo=%-8.3g" lo);
    if max then Buffer.add_string buf (Printf.sprintf "  hi=%-8.3g" hi);
    if mean then Buffer.add_string buf (Printf.sprintf "  mean=%-8.3g" mean_val);
    if med then Buffer.add_string buf (Printf.sprintf "  med=%-8.3g" median);
    (match q1 with
    | Some v -> Buffer.add_string buf (Printf.sprintf "  Q1=%-8.3g" v)
    | None -> ());
    (match q3 with
    | Some v -> Buffer.add_string buf (Printf.sprintf "  Q3=%-8.3g" v)
    | None -> ());
    Buffer.contents buf
  in
  B.(
    vlist ~bars:false
      [
        hlist ~bars:false [ text (Printf.sprintf "%-20s" label); text plot ];
        text (String.make 20 ' ' ^ stats);
      ])

let pp_gen_distr ?(rand : Random.State.t = Random.State.make [| 0 |])
    ?(n = 1000) ?(width = 88) ?(max = true) ?(min = true) ?(mean = true)
    ?(med = false) ?(lo_p = 0.05) ?(hi_p = 0.95) ?q1_p ?q3_p ?(colors = false)
    (gen : 'a QCheck2.Gen.t) (stats : 'a stat list) : string =
  let samples = QCheck2.Gen.generate ~rand ~n gen in
  let boxes =
    List.map
      (fun (label, f) ->
        let values = List.map f samples in
        boxplot_of_samples ~width ~max ~min ~mean ~med ~lo_p ~hi_p ?q1_p ?q3_p
          label values)
      stats
  in
  let report = B.(frame (vlist ~bars:false boxes)) in
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

    let leaf_count = size (* for this tree type, size counts leaves *)

    let internal_count t =
      size t - 1 (* nodes = leaves - 1 for full binary trees *)

    (* Depth of each leaf *)
    let leaf_depths t =
      let rec go depth = function
        | Leaf _ -> [ depth ]
        | Node (l, r) -> go (depth + 1) l @ go (depth + 1) r
      in
      go 0 t

    let min_depth t = leaf_depths t |> List.fold_left Int.min Int.max_int
    let max_depth t = height t (* same thing *)

    (* Is the tree perfectly balanced: all leaves at the same depth *)
    let is_perfect t =
      let depths = leaf_depths t in
      List.for_all (fun d -> d = List.hd depths) depths

    (* Balance factor: max_depth - min_depth (0 = perfect) *)
    let balance_factor t = max_depth t - min_depth t

    (* Width at each level: how many nodes at depth d *)
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

    (* Left/right spine lengths *)
    let rec left_spine = function
      | Leaf _ -> 0
      | Node (l, _) -> 1 + left_spine l

    let rec right_spine = function
      | Leaf _ -> 0
      | Node (_, r) -> 1 + right_spine r

    (* Is the tree left- or right-leaning at every node *)
    let rec is_left_skewed = function
      | Leaf _ -> true
      | Node (l, r) ->
          height l >= height r && is_left_skewed l && is_left_skewed r

    (* Path lengths: sum of depths of all leaves -- measures average search cost *)
    let total_path_length t = leaf_depths t |> List.fold_left ( + ) 0

    let avg_leaf_depth t =
      let leaves = leaf_count t in
      Float.of_int (total_path_length t) /. Float.of_int leaves

    let tree_stats : tree stat list =
      let float_of_bool = fun b -> if b then 1.0 else -1.0 in
      [
        ("height", fun tree -> height tree |> float_of_int);
        ("leaf_count", fun tree -> leaf_count tree |> float_of_int);
        ("internal_count", fun tree -> internal_count tree |> float_of_int);
        ("min_depth", fun tree -> min_depth tree |> float_of_int);
        ("max_depth", fun tree -> max_depth tree |> float_of_int);
        ("is_perfect", fun tree -> is_perfect tree |> float_of_bool);
        ("balance_factor", fun tree -> balance_factor tree |> float_of_int);
        ("max_width", fun tree -> max_width tree |> float_of_int);
        ("left_spine", fun tree -> left_spine tree |> float_of_int);
        ("right_spine", fun tree -> right_spine tree |> float_of_int);
        ("is_left_skewed", fun tree -> is_left_skewed tree |> float_of_bool);
        ("avg_leaf_depth", fun tree -> avg_leaf_depth tree);
        ("total_path_length", fun tree -> total_path_length tree |> float_of_int);
      ]

    let%expect_test _ =
      let rand = Random.State.make [| 0 |] in
      let report = pp_gen_distr ~rand ~n:1000 g tree_stats in
      print_endline report;

      [%expect {|
        ┌────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
        │height              |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=10        mean=2.9                                              │
        │leaf_count          |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=1         hi=143       mean=26.5                                             │
        │internal_count      |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=142       mean=25.5                                             │
        │min_depth           |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=2         mean=0.935                                            │
        │max_depth           |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=10        mean=2.9                                              │
        │is_perfect          |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=-1        hi=1         mean=0.052                                            │
        │balance_factor      |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=9         mean=1.96                                             │
        │max_width           |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=1         hi=40        mean=8.08                                             │
        │left_spine          |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=5         mean=1.5                                              │
        │right_spine         |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=5         mean=1.53                                             │
        │is_left_skewed      |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=-1        hi=1         mean=0.242                                            │
        │avg_leaf_depth      |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=4.31      mean=1.29                                             │
        │total_path_length   |--------------------------------------------------------------------------------------|│
        │                    n=1000  lo=0         hi=636       mean=103                                              │
        └────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
        |}]
  end)
