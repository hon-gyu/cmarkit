(** {0:distr Visualise QCheck2 generator distributions} *)

module B = PrintBox

let percentile (sorted : int array) (p : float) : int =
  let n = Array.length sorted in
  let i = int_of_float (float_of_int n *. p) in
  sorted.(Int.min i (n - 1))

(** Draw a single box-plot row as a string of [width] chars, given integer
    positions already mapped to [0..width]. *)
let render_boxplot ~width ~lo ~q1 ~med ~q3 ~hi =
  let line = Bytes.make width '-' in
  (* clear interior of box *)
  for i = q1 to q3 do
    Bytes.set line i ' '
  done;
  Bytes.set line lo '|';
  Bytes.set line hi '|';
  Bytes.set line q1 '[';
  Bytes.set line q3 ']';
  Bytes.set line med '|';
  Bytes.to_string line

let boxplot_of_samples ?(width = 88) ?(lo_p = 0.05) ?(hi_p = 0.95)
    (label : string) (samples : int list) : B.t =
  let arr = Array.of_list samples in
  Array.sort Int.compare arr;
  let lo = percentile arr lo_p in
  let q1 = percentile arr 0.25 in
  let med = percentile arr 0.50 in
  let q3 = percentile arr 0.75 in
  let hi = percentile arr hi_p in
  (* map value to column *)
  let range = hi - lo in
  let col v =
    if range = 0 then width / 2
    else Int.min (width - 1) ((v - lo) * (width - 1) / range)
  in
  let plot =
    render_boxplot ~width ~lo:(col lo) ~q1:(col q1) ~med:(col med) ~q3:(col q3)
      ~hi:(col hi)
  in
  let stats =
    Printf.sprintf "p5=%-5d Q1=%-5d med=%-5d Q3=%-5d p95=%-5d  n=%d" lo q1 med
      q3 hi (Array.length arr)
  in
  B.(
    vlist ~bars:false
      [
        hlist ~bars:false [ text (Printf.sprintf "%-20s" label); text plot ];
        text (String.make 20 ' ' ^ stats);
      ])

let pp_gen_distr ?(n = 1000) ?(width = 88) ?(lo_p = 0.05) ?(hi_p = 0.95)
    ?(colors = false) (gen : 'a QCheck2.Gen.t)
    (stats : (string * ('a -> int)) list) : string =
  let samples = QCheck2.Gen.generate ~n gen in
  let boxes =
    List.map
      (fun (label, f) ->
        let values = List.map f samples in
        boxplot_of_samples ~width ~lo_p ~hi_p label values)
      stats
  in
  let report = B.(frame (vlist ~bars:false boxes)) in
  PrintBox_text.to_string_with ~style:colors report
