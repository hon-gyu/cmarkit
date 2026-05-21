open Oymarkit_

type result =
  | Pass
  | Fail of { reason : string; expected : string; actual : string }

type t = { name : string; check : Block.t -> result }

(* Helpers
=========== *)

let canonical b =
  Format.asprintf "%a" Pp.pp_block (Block.normalize b)

let block_equal a b = String.equal (canonical a) (canonical b)

let render b =
  let doc = Doc.make b in
  Cmarkit_commonmark.of_doc doc

let reparse b =
  let s = render b in
  Doc.block (Doc.of_string s)

let fail ~reason ~expected ~actual = Fail { reason; expected; actual }

let check_eq ~reason ~expected ~actual =
  if block_equal expected actual then Pass
  else fail ~reason
        ~expected:(canonical expected)
        ~actual:(canonical actual)

(* Properties
  =========== *)

let roundtrip =
  { name = "roundtrip";
    check =
      fun b ->
        let b' = reparse b in
        check_eq ~reason:"parse (render b) /= b" ~expected:b ~actual:b' }

let normalize_idempotent =
  { name = "normalize_idempotent";
    check =
      fun b ->
        let n1 = Block.normalize b in
        let n2 = Block.normalize n1 in
        check_eq ~reason:"normalize is not idempotent" ~expected:n1 ~actual:n2 }

let render_determinism =
  { name = "render_determinism";
    check =
      fun b ->
        let s1 = render b in
        let s2 = render (reparse b) in
        if String.equal s1 s2 then Pass
        else fail ~reason:"render is not stable under reparse"
              ~expected:s1 ~actual:s2 }

(* Container uniformity
   --------------------- *)

let wrap_block_quote b =
  Block.Block_quote (Block.Block_quote.make b, Meta.none)

let wrap_list_item b =
  let item = (Block.List_item.make b, Meta.none) in
  let l = Block.List'.make (`Unordered '-') [item] in
  Block.List (l, Meta.none)

(* Unwrap to the inner block, or None if outer shape changed. *)
let unwrap_block_quote = function
  | Block.Block_quote (bq, _) -> Some (Block.Block_quote.block bq)
  | _ -> None

let unwrap_list_item = function
  | Block.List (l, _) ->
      (match Block.List'.items l with
       | [ (item, _) ] -> Some (Block.List_item.block item)
       | _ -> None)
  | _ -> None

(* After reparse the top-level block is typically [Blocks [single]]; peel it. *)
let peel_singleton_blocks = function
  | Block.Blocks ([ b ], _) -> b
  | b -> b

let uniformity_with ~name ~wrap ~unwrap =
  { name;
    check =
      fun b ->
        let wrapped = wrap b in
        let parsed = reparse wrapped |> peel_singleton_blocks in
        match unwrap parsed with
        | None ->
            fail ~reason:"container shape not preserved by round-trip"
              ~expected:(canonical wrapped) ~actual:(canonical parsed)
        | Some inner ->
            check_eq ~reason:"container content not preserved"
              ~expected:b ~actual:inner }

let uniformity_block_quote =
  uniformity_with ~name:"uniformity/block_quote"
    ~wrap:wrap_block_quote ~unwrap:unwrap_block_quote

let uniformity_list_item =
  uniformity_with ~name:"uniformity/list_item"
    ~wrap:wrap_list_item ~unwrap:unwrap_list_item

let all =
  [ roundtrip;
    normalize_idempotent;
    render_determinism;
    uniformity_block_quote;
    uniformity_list_item ]

let%expect_test "roundtrip on a small fixture" =
  let md = "# Hello\n\nfoo bar\n" in
  let b = Doc.block (Doc.of_string md) in
  let r = roundtrip.check b in
  (match r with
   | Pass -> print_endline "Pass"
   | Fail { reason; _ } -> Printf.printf "Fail: %s\n" reason);
  [%expect {| Pass |}]
