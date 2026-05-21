(* Wire the property battery to the surface-AST generator via QCheck2 *)

let print_s_block s =
  Sexplib0.Sexp.to_string_hum (Gen_block.sexp_of_s_block s)

let qcheck_of (prop : Property.t) : QCheck2.Test.t =
  QCheck2.Test.make
    ~name:prop.name
    ~print:print_s_block
    Gen_block.gen_block
    (fun s ->
       let b = Gen_block.to_block s in
       match prop.check b with
       | Property.Pass -> true
       | Property.Fail { reason; expected; actual } ->
           QCheck2.Test.fail_reportf
             "@[<v>%s@,@[<2>expected:@ %s@]@,@[<2>actual:@ %s@]@]"
             reason expected actual)

(* All properties as QCheck2 tests. *)
let tests = List.map qcheck_of Property.all

(* Convenience: run a single property to first failure, return result. *)
type outcome =
  | Ok_passed of { name : string; count : int }
  | Ko_failed of { name : string; witness : string; report : string }

let run_one ?(count = 100) ?(rand = Random.State.make_self_init ()) prop
  : outcome =
  let captured = ref None in
  let cell =
    QCheck2.Test.make_cell
      ~name:prop.Property.name ~count
      ~print:print_s_block
      Gen_block.gen_block
      (fun s ->
         let b = Gen_block.to_block s in
         match prop.Property.check b with
         | Property.Pass -> true
         | Property.Fail { reason; expected; actual } ->
             captured := Some (s, reason, expected, actual);
             false)
  in
  let res = QCheck2.Test.check_cell ~rand cell in
  match QCheck2.TestResult.get_state res, !captured with
  | QCheck2.TestResult.Success, _ ->
      Ok_passed { name = prop.name; count }
  | _, Some (s, reason, expected, actual) ->
      Ko_failed
        { name = prop.name;
          witness = print_s_block s;
          report =
            Printf.sprintf "%s\nexpected: %s\nactual: %s"
              reason expected actual }
  | _, None ->
      Ko_failed
        { name = prop.name; witness = "<unknown>";
          report = "test failed without a captured witness" }

let run_all ?count ?rand () =
  let run = match rand with
    | None -> fun prop -> run_one ?count prop
    | Some rand -> fun prop -> run_one ?count ~rand prop
  in
  List.map run Property.all

(* Pretty-print outcomes one per line, with a trailing summary. *)
let pp_outcomes ppf (outcomes : outcome list) =
  let nfail = ref 0 in
  List.iter (fun o ->
    match o with
    | Ok_passed { name; count } ->
        Format.fprintf ppf "[PASS] %s (%d cases)@," name count
    | Ko_failed { name; witness; report } ->
        incr nfail;
        Format.fprintf ppf
          "@[<v 2>[FAIL] %s@,witness:@,%s@,@,%s@]@,"
          name witness report) outcomes;
  Format.fprintf ppf "@,%d/%d passed@."
    (List.length outcomes - !nfail) (List.length outcomes)

let%expect_test _ =
  let rand = Random.State.make [| 42 |] in
  let outcomes = run_all ~count:30 ~rand () in
  Format.printf "@[<v>%a@]" pp_outcomes outcomes;
  [%expect {|
    [FAIL] roundtrip
      witness:
      (Ulist (((Para ((Text a))) (Para ((Text a))))))

      parse (render b) /= b
    expected: List { unordered '-'; tight }
      - item
        Blocks
          Paragraph "a"
          Paragraph "a"
    actual: List { unordered '-'; tight }
      - item
        Paragraph "a a"
    [PASS] normalize_idempotent (30 cases)
    [FAIL] render_determinism
      witness:
      (Heading 1 ((Text a) (Emph ((Code a)))))

      render is not stable under reparse
    expected: # a*`a`*
    actual: # a\*`a`\*
    [FAIL] uniformity/block_quote
      witness:
      (Para ((Text a) (Code a) (Code a)))

      container content not preserved
    expected: Paragraph "aaa"
    actual: Paragraph "aa``a"
    [FAIL] uniformity/list_item
      witness:
      (Heading 1 ((Text a) (Emph ((Code a)))))

      container content not preserved
    expected: Heading H1 "aa"
    actual: Heading H1 "a*a*"

    1/5 passed
    |}]
