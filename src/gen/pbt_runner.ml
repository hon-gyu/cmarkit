(* open Oymarkit_

(* Wire the property battery to the generator via QCheck2 *)

let print_block b = Format.asprintf "%a" Pp.pp_block b

let test_of_prop (prop : Property.t) : QCheck2.Test.t =
  QCheck2.Test.make ~name:prop.name ~print:print_block Gen_block.gen_block
    (fun b ->
      match prop.check b with
      | Property.Pass -> true
      | Property.Fail { reason; expected; actual } ->
          QCheck2.Test.fail_reportf
            "@[<v>%s@,@[<2>expected:@ %s@]@,@[<2>actual:@ %s@]@]" reason
            expected actual)

let tests = List.map test_of_prop Property.all

type outcome =
  | Ok_passed of { name : string; count : int }
  | Ko_failed of { name : string; witness : string; report : string }

let run_one ?(count = 100) ?(rand = Random.State.make_self_init ()) prop :
    outcome =
  let captured = ref None in
  let cell =
    QCheck2.Test.make_cell ~name:prop.Property.name ~count ~print:print_block
      Gen_block.gen_block (fun b ->
        match prop.Property.check b with
        | Property.Pass -> true
        | Property.Fail { reason; expected; actual } ->
            captured := Some (b, reason, expected, actual);
            false)
  in
  let res = QCheck2.Test.check_cell ~rand cell in
  match (QCheck2.TestResult.get_state res, !captured) with
  | QCheck2.TestResult.Success, _ -> Ok_passed { name = prop.name; count }
  | _, Some (b, reason, expected, actual) ->
      Ko_failed
        {
          name = prop.name;
          witness = print_block b;
          report =
            Printf.sprintf "%s\nexpected: %s\nactual: %s" reason expected actual;
        }
  | _, None ->
      Ko_failed
        {
          name = prop.name;
          witness = "<unknown>";
          report = "test failed without a captured witness";
        }

let run_all ?count ?rand () =
  let run =
    match rand with
    | None -> fun prop -> run_one ?count prop
    | Some rand -> fun prop -> run_one ?count ~rand prop
  in
  List.map run Property.all

let pp_outcomes ppf (outcomes : outcome list) =
  let nfail = ref 0 in
  List.iter
    (fun o ->
      match o with
      | Ok_passed { name; count } ->
          Format.fprintf ppf "[PASS] %s (%d cases)@," name count
      | Ko_failed { name; witness; report } ->
          incr nfail;
          Format.fprintf ppf "@[<v 2>[FAIL] %s@,witness:@,%s@,@,%s@]@," name
            witness report)
    outcomes;
  Format.fprintf ppf "@,%d/%d passed@."
    (List.length outcomes - !nfail)
    (List.length outcomes)

let%expect_test _ =
  let rand = Random.State.make [| 42 |] in
  let outcomes = run_all ~count:30 ~rand () in
  Format.printf "@[<v>%a@]" pp_outcomes outcomes;
  [%expect {|
    [FAIL] roundtrip
      witness:
      List { unordered '-'; tight }
      - item
        Blocks
          Paragraph "alpha"
          Paragraph "alpha"

      parse (render b) /= b
    expected: List { unordered '-'; tight }
      - item
        Blocks
          Paragraph "alpha"
          Paragraph "alpha"
    actual: List { unordered '-'; tight }
      - item
        Paragraph "alpha alpha"
    [PASS] normalize_idempotent (30 cases)
    [FAIL] render_determinism
      witness:
      Block_quote
      Block_quote
        Blocks
          Paragraph "alpha"
          Block_quote
            Paragraph "alphaid"

      render is not stable under reparse
    expected: > > alpha
    > > > alpha*`id`*
    actual: > > alpha
    > > > alpha\*`id`\*
    [FAIL] uniformity/block_quote
      witness:
      List { unordered '-'; tight }
      - item
        Heading H1 "idid"

      container content not preserved
    expected: List { unordered '-'; tight }
      - item
        Heading H1 "idid"
    actual: List { unordered '-'; tight }
      - item
        Heading H1 "id``id"
    [FAIL] uniformity/list_item
      witness:
      Thematic_break

      container shape not preserved by round-trip
    expected: List { unordered '-'; tight }
      - item
        Thematic_break
    actual: Thematic_break

    1/5 passed
    |}] *)
