(* Djot conformance: run the djot.js test corpus (vendor/djot.js/test) against
   the [djot] preset and report, per feature file, how many examples we render
   exactly as djot does.

   The score is checked in as [djot_conformance.expected] rather than pinned to a
   single number, because unlike the CommonMark run this one is far from perfect:
   a per-file table says *where* we stand and, when it moves, *which* feature
   moved. Improving a feature is a [dune promote] away; a regression shows up as
   a one-line diff naming the file it broke.

   Corpus format (see djot.js/src/functional.spec.ts): a test is a run of
   backticks whose info string is an options string, then the input, then a line
   holding only [.] or [!], then the expected output, then a closing fence of the
   same backtick run. Two options are not about HTML and are skipped rather than
   counted as failures:

   - [a]: the expected output is djot's own AST dump, in djot.js's format. We
     have no reason to reproduce that format.
   - [p]: source positions are to be rendered into the output. We have locations,
     but not this serialisation of them.

   [filters.test] is excluded: it tests djot.js's filter API, not the syntax, and
   djot.js's own runner leaves it out too. *)

module C = Cmarkit_

let corpus_dir = "../vendor/djot.js/test"

(* djot.js's own list, which is the set of files it considers the syntax corpus. *)
let files =
  [ "attributes.test"; "block_quote.test"; "code_blocks.test";
    "definition_lists.test"; "symb.test"; "emphasis.test"; "escapes.test";
    "fenced_divs.test"; "footnotes.test"; "headings.test";
    "insert_delete_mark.test"; "links_and_images.test"; "lists.test";
    "math.test"; "para.test"; "raw.test"; "regression.test"; "smart.test";
    "spans.test"; "sourcepos.test"; "super_subscript.test"; "tables.test";
    "task_lists.test"; "thematic_breaks.test"; "verbatim.test" ]

type test =
  { line : int (* of the opening fence, for locating a failure *);
    options : string;
    input : string;
    output : string }

let read_lines path =
  let ic = open_in_bin path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  loop []

let is_fence line =
  let n = ref 0 in
  while !n < String.length line && line.[!n] = '`' do incr n done;
  if !n >= 3 then Some (String.sub line 0 !n, String.trim (String.sub line !n
                          (String.length line - !n)))
  else None

let is_separator line = String.equal line "." || String.equal line "!"

(* Cut a corpus file into its tests. Text between tests is prose and ignored. *)
let parse_tests path =
  let lines = read_lines path in
  let rec skip_to_fence n = function
    | [] -> None
    | line :: rest -> (
        match is_fence line with
        | Some (ticks, options) -> Some (ticks, options, n, rest)
        | None -> skip_to_fence (n + 1) rest)
  in
  let rec take_until stop n acc = function
    | [] -> (List.rev acc, n, [])
    | line :: rest when stop line -> (List.rev acc, n + 1, rest)
    | line :: rest -> take_until stop (n + 1) (line :: acc) rest
  in
  let rec loop n lines acc =
    match skip_to_fence n lines with
    | None -> List.rev acc
    | Some (ticks, options, fence_line, rest) ->
        let input, n, rest = take_until is_separator (fence_line + 1) [] rest in
        let closes line =
          String.length line >= String.length ticks
          && String.equal (String.sub line 0 (String.length ticks)) ticks
        in
        let output, n, rest = take_until closes n [] rest in
        let cat ls = String.concat "" (List.map (fun l -> l ^ "\n") ls) in
        let t =
          { line = fence_line + 1; options; input = cat input;
            output = cat output }
        in
        loop n rest (t :: acc)
  in
  loop 0 lines []

let skipped t =
  String.exists (fun c -> c = 'a' || c = 'p') t.options

let render input =
  (* The whole point of the preset: djot conformance should be one expression. *)
  Cmarkit_html.of_doc ~safe:false (C.Doc.of_string ~djot:true input)

let run_file file =
  let tests = parse_tests (Filename.concat corpus_dir file) in
  List.fold_left
    (fun (pass, total, skip, failures) t ->
      if skipped t then (pass, total, skip + 1, failures)
      else
        let got = try render t.input with e -> "EXCEPTION: " ^ Printexc.to_string e in
        if String.equal got t.output then (pass + 1, total + 1, skip, failures)
        else (pass, total + 1, skip, (t, got) :: failures))
    (0, 0, 0, []) tests

(* [DJOT_CONFORMANCE_SHOW=file.test] dumps that file's failures, which is how you
   work on one. It is not part of the recorded output. *)
let show_failures = Sys.getenv_opt "DJOT_CONFORMANCE_SHOW"

let dump_failures file failures =
  let show t got =
    Printf.eprintf "--- %s:%d (options %S)\n%s---- expected\n%s---- got\n%s\n"
      file t.line t.options t.input t.output got
  in
  List.iter (fun (t, got) -> show t got) (List.rev failures)

let () =
  let rows = List.map (fun file -> (file, run_file file)) files in
  let tpass, ttotal, tskip =
    List.fold_left
      (fun (a, b, c) (_, (pass, total, skip, _)) -> (a + pass, b + total, c + skip))
      (0, 0, 0) rows
  in
  List.iter
    (fun (file, (pass, total, skip, failures)) ->
      let skip = if skip = 0 then "" else Printf.sprintf "  (%d skipped)" skip in
      Printf.printf "%-24s %3d/%-3d%s\n" file pass total skip;
      match show_failures with
      | Some f when String.equal f file -> dump_failures file failures
      | _ -> ())
    rows;
  Printf.printf "%-24s %3d/%-3d  (%d skipped)\n" "TOTAL" tpass ttotal tskip
