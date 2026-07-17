(* CommonMark conformance: parse every example in the official spec and compare
   our HTML to the spec's.

   The point of this test is not the absolute score — the fork carries a known
   set of mismatches, pinned below — but that the score does not move when a
   default-off knob is added. It sweeps two axes that a naive conformance run
   misses:

   - [enable_oymarkit]. The older probe pinned this to [false], which skips every
     gated inline branch: a new inline feature would then never be entered, and
     the run would clear it without ever executing it. Real users get [true] with
     the feature knobs simply left at their defaults.
   - [strict]. Extensions off ([true]) and on ([false]).

   All four cells must agree: a knob that is off must cost nothing. If you add a
   knob and this test fails, the knob is not really off by default. *)

module C = Cmarkit_

(* The fork's known-mismatch count. This is a *pin*, not a target: it exists so
   that a change which alters CommonMark behaviour has to say so out loud. *)
let known_mismatches = 58

let spec_path = "./spec.json"

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let examples () =
  match Yojson.Safe.from_string (read_file spec_path) with
  | `List l -> l
  | _ -> []

let field ex k =
  match ex with
  | `Assoc fields -> (
      match List.assoc_opt k fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let mismatches ~render ~oymarkit ~strict examples =
  C.Parser_common.set_enable_oymarkit oymarkit;
  let fail =
    List.fold_left
      (fun fail ex ->
        let md = field ex "markdown" and expected = field ex "html" in
        let got = render (C.Doc.of_string ~strict md) in
        if String.equal got expected then fail else fail + 1)
      0 examples
  in
  C.Parser_common.set_enable_oymarkit true;
  fail

(* The default renderer emits HTML5 void elements ([<br>], [<hr>], [<img …>]);
   the spec's reference HTML uses the XHTML spelling ([<br />]), so the fork
   carries [known_mismatches] purely from that one rendering choice. *)
let html_render = Cmarkit_html.of_doc ~safe:false

(* The XHTML renderer closes void elements the spec's way. It exists to prove
   the divergence is *only* about rendering: parsing is byte-exact against the
   spec, so this path must be a perfect 0. If it ever moves, a real parsing
   regression has slipped in — which the HTML5 pin above could otherwise hide
   (one example breaking while another is fixed keeps the count at 58). *)
let xhtml_render =
  let r = Cmarkit_html.xhtml_renderer ~safe:false () in
  fun d -> Cmarkit_renderer.doc_to_string r d

let () =
  let examples = examples () in
  let total = List.length examples in
  assert (total > 0);
  let sweep ~render ~pin label =
    [ (false, true); (true, true); (false, false); (true, false) ]
    |> List.iter (fun (oymarkit, strict) ->
           let fail = mismatches ~render ~oymarkit ~strict examples in
           Printf.printf "%-6s oymarkit=%-5b strict=%-5b : %d/%d mismatches\n"
             label oymarkit strict fail total;
           assert (fail = pin))
  in
  (* The default renderer: every cell must agree with the pin, so a default-off
     knob costs nothing and the fork's conformance has not moved. *)
  sweep ~render:html_render ~pin:known_mismatches "html";
  (* The XHTML renderer: exact spec bytes, so parsing conformance is perfect. *)
  sweep ~render:xhtml_render ~pin:0 "xhtml";
  print_endline "all cells agree with their pins"
