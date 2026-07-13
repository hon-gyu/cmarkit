[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CommonMark parser and abstract syntax tree.

    See {{!page-index.quick}examples}.

    {b References.}
    {ul
    {- John MacFarlane.
    {e {{:https://spec.commonmark.org/0.31.2/}
    CommonMark Spec}}. Version 0.31.2, 2024}}  *)

(** {1:ast Abstract syntax tree} *)

(** Text locations.

    A text location identifies a text span in a given UTF-8 encoded file
    by an inclusive range of absolute {{!Textloc.type-byte_pos}byte} positions
    and the {{!Textloc.type-line_pos}line positions} on which those occur. *)
module Textloc : sig

  (** {1:fpath File paths} *)

  type fpath = string
  (** The type for file paths. *)

  val file_none : fpath
  (** [file_none] is ["-"]. A file path to use when there is none. *)

  (** {1:pos Positions} *)

  (** {2:byte_pos Byte positions} *)

  type byte_pos = int
  (** The type for zero-based, absolute, byte positions in text. If
      the text has [n] bytes, [0] is the first position and [n-1] is
      the last position. *)

  val byte_pos_none : byte_pos
  (** [byte_pos_none] is [-1]. A position to use when there is none. *)

  (** {2:lines Lines} *)

  type line_num = int
  (** The type for one-based, line numbers in the text. Lines
      increment after a {e newline} which is either a line feed ['\n']
      (U+000A), a carriage return ['\r'] (U+000D) or a carriage return
      and a line feed ["\r\n"] (<U+000D,U+000A>). *)

  val line_num_none : line_num
  (** [line_num_none] is [-1]. A line number to use when there is none. *)

  (** {2:line_pos Line positions} *)

  type line_pos = line_num * byte_pos
  (** The type for line positions. This identifies a line by its line
      number and the absolute byte position following its newline
      (or the start of text for the first line). That byte position:
      {ul
      {- Indexes the first byte of text of the line if the line is non-empty.}
      {- Indexes the first byte of the next newline if the line is empty.}
      {- Is out of bounds and equal to the text's length for a last empty
         line (this includes when the text is empty).}} *)

  val line_pos_first : line_pos
  (** [line_pos_first] is [1, 0]. Note that this is the only line position
      of the empty text. *)

  val line_pos_none : line_pos
  (** [line_pos_none] is [(line_none, pos_none)]. *)

  (** {1:tloc Text locations} *)

  type t
  (** The type for text locations. A text location identifies a text
      span in an UTF-8 encoded file by an inclusive range of absolute
      {{!type-byte_pos}byte positions} and the {{!type-line_pos}line positions}
      on which they occur.

      If the first byte equals the last byte the range contains
      exactly that byte. If the first byte is greater than the last
      byte this represents an insertion point before the first byte. In
      this case information about the last position should be ignored:
      it can contain anything. *)

  val none : t
  (** [none] is a position to use when there is none. *)

  val v :
    file:fpath -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t
  (** [v ~file ~first_byte ~last_byte ~first_line ~last_line] is a text
      location with the given arguments, see corresponding accessors for
      the semantics. If you don't have a file use {!file_none}. *)

  val file : t -> fpath
  (** [file l] is [l]'s file. *)

  val first_byte : t -> byte_pos
  (** [first_byte l] is [l]'s first byte. Irrelevant if {!is_none} is
      [true]. *)

  val last_byte : t -> byte_pos
  (** [last_byte l] is [l]'s last byte. Irrelevant if {!is_none} or {!is_empty}
      is [true]. *)

  val first_line : t -> line_pos
  (** [first_line l] is the line position on which [first_byte l] lies.
      Irrelevant if {!is_none} is [true].*)

  val last_line : t -> line_pos
  (** [last_line l] is the line position on which [last_byte l] lies.
      Irrelevant if {!is_none} or {!is_empty} is [true].*)

  (** {2:preds Predicates and comparisons} *)

  val is_none : t -> bool
  (** [is_none t] is [true] iff [first_byte < 0]. *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [first_byte t > last_byte t]. *)

  val equal : t -> t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] are equal. This checks
      that {!file}, {!first_byte} and {!last_byte} are equal. Line information
      is ignored. *)

  val compare : t -> t -> int
  (** [compare t0 t1] orders [t0] and [t1]. The order is compatible
      with {!equal}. Comparison starts with {!file}, follows with {!first_byte}
      and ends, if needed, with {!last_byte}. Line information is ignored. *)

  (** {2:shrink_and_stretch Shrink and stretch} *)

  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  (** [set_first l ~first_byte ~first_line] sets the the first position of
      [l] to given values. *)

  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  (** [set_last l ~last_byte ~last_line] sets the last position of [l]
      to given values. *)

  val to_first : t -> t
  (** [to_first l] has both first and last positions set to [l]'s first
      position. The range spans {!first_byte}. See also {!before}. *)

  val to_last : t -> t
  (** [to_last l] has both first and last positions set to [l]'s last
      position. The range spans {!last_byte}. See also {!after}. *)

  val before : t -> t
  (** [before t] is the {{!is_empty}empty} text location starting at
      {!first_byte}. *)

  val after : t -> t
  (** [after t] is the empty {{!is_empty}empty} location starting at
      [last_byte t + 1]; note that at the end of input this may be an
      invalid byte {e index}. The {!first_line} and {!last_line} of the
      result is [last_line t]. *)

  val span : t -> t -> t
  (** [span l0 l1] is the span from the smallest byte position of [l0] and
      [l1] to the largest byte position of [l0] and [l1]. The file path is
      taken from the greatest byte position. *)

  val reloc : first:t -> last:t -> t
  (** [reloc ~first ~last] uses the first position of [first], the
      last position of [last] and the file of [last]. *)

  (** {2:fmt Formatting} *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats text locations like the OCaml compiler. *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats text locations according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is {!pp_gnu}. *)

  val pp_dump : Format.formatter -> t -> unit
  (** [pp_dump] formats raw data for debugging. *)
end

(** Node metadata.

    Holds text locations and custom, client-defined metadata. *)
module Meta : sig

  type id = int
  (** The type for non-negative metadata identifiers. *)

  type t
  (** The type for abstract syntax tree node metadata. *)

  val none : t
  (** [none] is metadata for when there is none, its {!textloc} is
      {!Textloc.none}. *)

  val make : ?textloc:Textloc.t -> unit -> t
  (** [make textloc] is metadata with text location [textloc] (defaults
      to {!Textloc.none}) and a fresh identifier (see {!val-id}). *)

  val id : t -> id
  (** [id m] is an identifier for the metadata. Depending on how you
      process the abstract syntax tree this may become non-unique but
      the metadata values in an abstract syntax tree returned by
      {!Doc.of_string} with [locs:true] have distinct identifiers. *)

  val textloc : t -> Textloc.t
  (** [textloc m] is the source location of the syntactic construct [m]
      is attached to. *)

  val with_textloc : keep_id:bool -> t -> Textloc.t -> t
  (** [with_textloc ~keep_id m textloc] is metadata [m] with text location
      [textloc] and a fresh id, unless [keep_id] is [true]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal m0 m1] is [true] if [m0] and [m1] have the same {!val-id}.
      Note that they may have different {{!custom}metadata.} *)

  val compare : t -> t -> int
  (** [compare m0 m1] is a total order on metadata {!val-id}s compatible with
      {!equal}. *)

  val is_none : t -> bool
  (** [is_none m] is [equal none m]. *)

  (** {1:custom Custom metadata}

      {b Warning.} Operating on custom metadata never changes
      {!val-id}. It is possible for two meta values to have the same
      id and different metadata. *)

  type 'a key
  (** The type for custom metadata keys. *)

  val key : unit -> 'a key
  (** [key ()] is a new metadata key. *)

  val mem : 'a key -> t -> bool
  (** [mem k m] is [true] iff [k] is bound in [m]. *)

  val add : 'a key -> 'a -> t -> t
  (** [add k v m] is [m] with key [k] bound to [v]. *)

  val tag : unit key -> t -> t
  (** [tag k m] is [add k () m]. *)

  val remove : 'a key -> t -> t
  (** [remove k m] is [m] with key [k] unbound in [v]. *)

  val find : 'a key -> t -> 'a option
  (** [find k m] the value of [k] in [m], if any. *)
end

module Attribute : sig
  type t

  val empty : t
  val id : t -> string option
  val classes : t -> string list
  val key_values : t -> (string * string) list
  val source : t -> string option
  val merge : t -> t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

type 'a node = 'a * Meta.t
(** The type for abstract syntax tree nodes. The data of type ['a] and its
    metadata. *)

(** Types for layout information.

    Values of these types do not represent document data. They are
    used to recover document source layout informations when the
    abstract syntax tree cannot represent them.
    See {{!Cmarkit_commonmark.layout}source layout preservation}
    for more information.

    For programmatically generated nodes, values of these types can be
    left empty or filled with a desired layout. Except for the
    {{!Cmarkit_commonmark}CommonMark renderer} these values are usually
    ignored.  *)
module Layout : sig

  type blanks = string
  (** The type for blanks layout. This is only made of spaces and tabs. *)

  type nonrec string = string
  (** The type for string layout. For example the art of thematic breaks
      or code fences. *)

  type nonrec char = char
  (** The type for character layout. For example the character used for
      an emphasis or an unordered list marker. *)

  type count = int
  (** The type for some kind of layout count. Usually a character
      count. *)

  type indent = int
  (** The type for block indentation. Mostly between 0-3.  *)

  val string : ?meta:Meta.t -> string -> string node
  (** [string s] is a layout string with meta data [meta]
      (defaults to {!Meta.none}). *)

  val empty : string node
  (** [empty] is [string ""]. *)
end

(** Block lines.

    In CommonMark blocks, a "line" does not necessarily correspond to
    a line in the source plain text. For example the lines of a
    paragraph in a block quote are the lines stripped from the block
    quote markers.  We call the line resulting from stripping the
    block structure preceeding a given block a {e block line}. *)
module Block_line : sig

  (** {1:lines Lines} *)

  type t = string node
  (** The type for block lines. *)

  val to_string : t -> string
  (** [to_string l] is (fst l). *)

  val list_textloc : t list -> Textloc.t
  (** [list_textloc ls] is a text location spanning the lines [ls]
      This is {!Textloc.none} on [[]]. *)

  val list_of_string : ?meta:Meta.t -> string -> t list
  (** [list_of_string s] cuts [s] on newlines. [meta] is used for
      all nodes, default to [Meta.none]. *)

  (** {1:tight_lines Tight lines} *)

  type tight = Layout.blanks * t
  (** The type for tight block lines. A block line with its
      initial blanks trimmed but kept for layout. *)

  val tight_to_string : tight -> string
  (** [tight_to_string l] is [(fst (snd l))]. *)

  val tight_list_textloc : tight list -> Textloc.t
  (** [tigh_list_textloc ls] is a text location spanning the lines [ls]
      This is {!Textloc.none} on [[]]. *)

  val tight_list_of_string : ?meta:Meta.t -> string -> tight list
  (** [list_of_string s] cuts [s] on newlines and computes the blanks
      (except on the first line where they are part of the
      data). [meta] is used for all nodes, default to [Meta.none].  *)

  (** {1:blank_lines Blank lines} *)

  type blank = Layout.blanks node
  (** The type for blank block lines. *)
end

(** Labels.

    Labels are used by
    {{:https://spec.commonmark.org/0.31.2/#reference-link}reference links} to
    refer to the {{!Label.definitions}definitions} of
    {{:https://spec.commonmark.org/0.31.2/#link-reference-definitions}
    link reference definitions},
    {{!Cmarkit.ext_footnote_def}footnote definitions} and your own
    {{!Label.resolvers}interpretations}. *)
module Label : sig

  (** {1:label Labels} *)

  type key = string
  (** The type for label keys. These are
      {{:https://spec.commonmark.org/0.31.2/#link-label}link labels}
      normalized for {{:https://spec.commonmark.org/0.31.2/#matches}matching}. *)

  type t
  (** The type for {{:https://spec.commonmark.org/0.31.2/#link-label}link
      labels}. *)

  val make : ?meta:Meta.t -> key:string -> Block_line.tight list -> t
  (** [make key text] is a label with key [id] and unormalized text [text]. *)

  val with_meta : Meta.t -> t -> t
  (** [with_meta m l] is [l] with meta [m]. *)

  val meta : t -> Meta.t
  (** [meta k] is metadata for [k]. *)

  val key : t -> key
  (** [key_id l] is the label's key. If [l] comes out of a parse this
      [l]'s normalized {!text}. *)

  val text : t -> Block_line.tight list
  (** [text l] is the text of [l]. *)

  val text_to_string : t -> string
  (** [text_to_string l] is the lines of {!text} separated
          by spaces. In contrast to {!val-key} this has not gone
          throught {{:https://spec.commonmark.org/0.31.2/#matches}normalization}.
  *)

  val compare : t -> t -> int
  (** [compare l0 l1] is [String.compare (key l0) (key l1)]. *)

  (** {1:definitions Definitions}

      A label definition is the content referenced by its {!val-key}.

      Labels are defined in documents via footnotes and link reference
      definitions. Additional label definitions can be added before
      parsing starts by using the [defs] argument of
      {!Doc.of_string}. They can also be manipulated and
      created on the fly during parsing by using a
      {{!resolvers}resolver}. *)

  type def = ..
  (** The type for label definitions.
      See for example {!Link_definition.extension-Def} or
      {!Block.Footnote.extension-Def}. *)

  (** Label key maps. *)
  module Map : Map.S with type key := key

  type defs = def Map.t
  (** The type for label definitions. Maps label keys  to their definition. *)

  (** {1:resolvers Resolvers}

      To have more control over the label definitions used in a
      document, the [defs] argument of {!Doc.of_string} can be
      specified to pre-populate the label definitions used during parsing;
      for example with those of a previously parsed document.

      In addition the [resolver] argument can be specified to:
      {ol
      {- Alter or suppress label definitions made by link reference definitions
         and footnote definitions. It can also be used to warn, by
         side effect, on multiple label definitions.}
      {- Alter, or suppress label references on reference links and images –
         which happen after all label definitions have been made. You can
         define the actual label that will be used for resolving
         the reference to its definition.}}

      In particular 2. can be used to create synthetic label definitions
      on undefined label references. This provides the ability to treat
      the very liberal
      {{:https://spec.commonmark.org/0.31.2/#link-label}link label}
      syntax as a domain specific language of yours (e.g. for data binding).

      Note that parsing is not finished when resolvers are invoked
      this is the reason why you don't get access to the definition's
      data during resolution.

      See {{!resolver_example}an example}. *)

  type context =
  [ `Def of t option * t (** Label definitions *)
  | `Ref of [ `Link | `Image ] * t * t option (** Label references *) ]
  (** The type for resolver contexts. See {!type-resolver}. *)

  type resolver = context -> t option
  (** The type for resolvers. [context] is:
      {ul
      {- [`Def (prev, current)] when we just hit a
         {{:https://spec.commonmark.org/0.31.2/#link-reference-definitions}
         link reference definition} or
         {{!Cmarkit.ext_footnote_def}footnote definition} that defines
         the label [current]. If there is already a definition for
         [current]'s {!val-key} it is provided in [prev] (whose {!meta} has
         the location of the definition if you parse with locations).
         If [None] is returned the [current] definition is ignored,
         and definition [prev] (if any) is kept for the document.  If
         [Some l] is returned [l]'s key will be bound to the parsed
         definition for [current] in {!Doc.defs} at the end of parsing.
         The result of the resolver is stored in the abstract syntax tree and
         available via {!Link_definition.defined_label} and
         {!Block.Footnote.defined_label}.}
      {- [`Ref (kind, ref, def)] when we just hit a link or image
         referencing label [ref]. [def] is the label defining [ref]'s {!val-key}
         in the document (if any). The result of the resolver is the label
         stored for resolving the reference to its definition in the resulting
         {!Inline.module-Link} node;
         [None] means that [label] is undefined and the inline becomes
         {!Inline.extension-Text} like in CommonMark.}}

      See {{!resolver_example}an example} and the {!default_resolver}. *)

  val default_resolver : resolver
  (** [default_resolver] is the default resolver.

      This resolves according to the CommonMark specification.
      The first label definition always takes over subsequent
      ones and resolution is left untouched (i.e. a label has to be
      defined in the document to be used):
{[
let default_resolver = function
| `Def (None, l) -> Some l
| `Def (Some _, _) -> None (* Previous takes over *)
| `Ref (_, _, def) -> def
]} *)

  (** {1:resolver_example Resolver example}

      In this example we assume references to undefined labels denote
      links to pages or media in our wiki and want to process them
      them later via a {{!Mapper}tree transformation} or in a
      {{!Cmarkit_renderer.example}renderer extension}.

      We devise a resolver to create synthetic labels on any undefined
      label so that the CommonMark parser does not turn them into text.
{[
let wikilink = Cmarkit.Meta.key () (* A meta key to recognize them *)

let make_wikilink label = (* Just a placeholder label definition *)
  let meta = Cmarkit.Meta.tag wikilink (Cmarkit.Label.meta label) in
  Cmarkit.Label.with_meta meta label

let with_wikilinks = function
| `Def _ as ctx -> Cmarkit.Label.default_resolver ctx
| `Ref (_, _, (Some _ as def)) -> def (* As per doc definition *)
| `Ref (_, ref, None) -> Some (make_wikilink ref)
]}
  *)
end

(** Link definitions. *)
module Link_definition : sig

  (** {1:layout Layout} *)

  type layout =
    { indent : Layout.indent; (** Amount of indentation, [0] on inline links. *)
      angled_dest : bool; (** [true] if destination is between [<…>]. *)
      before_dest : Block_line.blank list; (** Blanks to destination. *)
      after_dest : Block_line.blank list; (** Blanks after destination. *)
      title_open_delim : Layout.char;
      (** Title open delimiter (['\"'], ['('], …)  *)
      after_title : Block_line.blank list;
      (** Blanks after title (inline links). *) }
  (** The type for link reference layout. *)

  val layout_for_dest : string -> layout
  (** [layout_for_dest d] computes a layout value for destination [d]. This
      just determines if [angled_dest] needs to be [true]. *)

  (** {1:link_defs Link definitions} *)

  type t
  (** The type for representing
      {{:https://spec.commonmark.org/0.31.2/#link-reference-definitions}
      link references definitions} and
      {{:https://spec.commonmark.org/0.31.2/#inline-link}inline links}. *)

  val make :
    ?layout:layout -> ?defined_label:Label.t option -> ?label:Label.t ->
    ?dest:string node -> ?title:Block_line.tight list -> unit -> t
  (** [make ()] is a link reference with given parameters. If [dest] is
      given and [layout] is not, the latter is computed with
      {!layout_for_dest}. [label] is a label if the link is defined
      via a link reference definition. [defined_label] defaults to
      [label]. *)

  val layout : t -> layout
  (** [layout ld] is the layout of [ld]. *)

  val label : t -> Label.t option
  (** [label ld] is [None] if this is a link definition for an inline
      link.  It is [Some l], if [ld] is a link reference
      definition. [l] is the label as found in the text. The result
      of the resolver is in {!defined_label}. *)

  val defined_label : t -> Label.t option
  (** [defined_label ld] is the label determined by the {!Label.type-resolver}
      for the link definition reference. The label as found
      in the source text is in {!label}. If this is [None] either
      it's a link definition for an inline link or the resolver deleted
      the label definition. *)

  val dest : t -> string node option
  (** [dest ld] is the link destination of [ld]. [None] means
      there was no destination. CommonMark renders that as an empty
      [href] in HTML. *)

  val title : t -> Block_line.tight list option
  (** [title ld] is the title of the reference, if any. *)

  (** {1:labeldef As label definitions} *)

  type Label.def += Def of t node (** *)
  (** A label definition for links. *)
end

(** Inlines.

    {b Note.} Document data in inline nodes is always stored
    {{:https://spec.commonmark.org/0.31.2/#backslash-escapes}unescaped} and
    with {{:https://spec.commonmark.org/0.31.2/#entity-and-numeric-character-references}entity and character references} resolved. *)
module Inline : sig

  (** {1:inlines Inlines} *)

  type t = ..
  (** The type for inlines. *)

  (** Autolinks. *)
  module Autolink : sig
    type t
    (** The type for
        {{:https://spec.commonmark.org/0.31.2/#autolink}autolinks}. *)

    val make : string node -> t
    (** [autolink link] is an autolink for [link]
        which must be a CommonMark
        {{:https://spec.commonmark.org/0.31.2/#absolute-uri}absolute URI}
        or a CommonMark
        {{:https://spec.commonmark.org/0.31.2/#email-address}email
        address}. *)

    val is_email : t -> bool
    (** [is_email a] is [true] iff {!link}[ a] is
        a CommonMark
        {{:https://spec.commonmark.org/0.31.2/#email-address}email
        address}. *)

    val link : t -> string node
    (** [link a] is the CommonMark
        {{:https://spec.commonmark.org/0.31.2/#absolute-uri}absolute URI} or
        {{:https://spec.commonmark.org/0.31.2/#email-address}email address}. *)
  end

  (** Hard and soft breaks *)
  module Break : sig

    type type' =
      [ `Hard (** {{:https://spec.commonmark.org/0.31.2/#hard-line-breaks}
                  Hard line break.} *)
      | `Soft (** {{:https://spec.commonmark.org/0.31.2/#soft-line-breaks}
                  Soft line break.} *) ]
    (** The type for types of line breaks. *)

    type t
    (** The type for
        {{:https://spec.commonmark.org/0.31.2/#hard-line-breaks}hard}
        and
        {{:https://spec.commonmark.org/0.31.2/#soft-line-breaks}soft}
        line breaks. *)

    val make :
      ?layout_before:Layout.string node -> ?layout_after:Layout.blanks node ->
      type' -> t
    (** [make type'] is a new break of type [type']. Layout values default
        to {!Layout.empty}. *)

    val type' : t -> type'
    (** [type' b] is the type of [b]. *)

    val layout_before : t -> Layout.string node
    (** [layout_before b] is the layout before the newline, spaces
        or possibly ['\'] for hard breaks. *)

    val layout_after : t -> Layout.blanks node
    (** [layout_after] are blanks on the new {e block line}. *)
  end

  (** Code spans. *)
  module Code_span : sig

    type t
    (** The type for
          {{:https://spec.commonmark.org/0.31.2/#code-spans}code spans}. *)

    val make : backtick_count:Layout.count -> Block_line.tight list -> t
    (** [make ~backtick_count code_layout] is a code span with given
        parameters.

        {b Warning.} Nothing is made to ensure correctness of the
        data, use {!of_string} to compute the right amount of
        backticks. *)

    val of_string : ?meta:Meta.t -> string -> t
    (** [of_string s] is a code span for [s]. [s] can start with or
        include backticks; the appropriate minimal backtick count and
        possible needed leading and trailing space are computed
        accordingly. If [s] contains newlines, blanks after newlines
        are treated as layout like during parsing. [meta] is used for
        the lines of the resulting code layout (see {!code_layout}). *)

    val backtick_count : t -> Layout.count
    (** [backtick_count cs] is the number of delimiting backticks. *)

    val code : t -> string
    (** [code cs] computes from {!code_layout} the code in the span [cs]. *)

    val code_layout : t -> Block_line.tight list
    (** [code_layout cs] is the code data in a form that allows layout
        preservation.

        The actual code data is the tight block lines concatenated and
        separated by space and if the result starts and ends with a
        space and is not only made of spaces, these should be
        dropped. The {!code} function does all that for you. *)
  end

  (** Emphasis and strong emphasis. *)
  module Emphasis : sig

    type inline := t

    type t
    (** The type for
        {{:https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis}
        emphasis and strong emphasis}. *)

    val make :
      ?delim:Layout.char -> ?open_marker:bool -> ?close_marker:bool ->
      inline -> t
    (** [make i] is an emphasis on [i]. [delim] is the delimiter
        used it should be either ['*'] or ['_']. If [open_marker] is [true],
        the CommonMark renderer emits an opening marker before the opening
        delimiter. If [close_marker] is [true], it emits a closing marker after
        the closing delimiter. Both default to [false]. *)

    val inline : t -> inline
    (** [inline e] is the emphasised inline. *)

    val delim : t -> Layout.char
    (** [delim e] is the delimiter used for emphasis, should be
        either ['*'] or ['_']. *)

    val open_marker : t -> bool
    (** [open_marker e] is [true] if [e] uses an opener marker. *)

    val close_marker : t -> bool
    (** [close_marker e] is [true] if [e] uses a closer marker. *)
  end

  (** Links. *)
  module Link : sig

    type inline := t

    type reference_layout =
    [ `Collapsed
      (** {{:https://spec.commonmark.org/0.31.2/#collapsed-reference-link}
          Collapsed reference link} *)
    | `Full
      (** {{:https://spec.commonmark.org/0.31.2/#full-reference-link}
          Full reference link} *)
    | `Shortcut
      (** {{:https://spec.commonmark.org/0.31.2/#shortcut-reference-link}
          Shortcut reference link} *) ]
    (** The type for reference link layouts. *)

    type reference =
    [ `Inline of Link_definition.t node
       (** {{:https://spec.commonmark.org/0.31.2/#inline-link}Inline link} *)
    | `Ref of reference_layout * Label.t * Label.t
       (** {{:https://spec.commonmark.org/0.31.2/#reference-link}Reference
           links}. First label is the label of the reference, second
           label is the label of the referenced definition. *) ]
    (** The type for references. *)

    type t
    (** The type for {{:https://spec.commonmark.org/0.31.2/#links}links}
        and {{:https://spec.commonmark.org/0.31.2/#images}images}.  *)

    val make : inline -> reference -> t
    (** [make i ref] is a link for text [i] and link reference [ref].

        If you plan to render to CommonMark and this is not an inline
        reference you should include a
        {!Block.extension-Link_reference_definition} (or
        {!Block.extension-Ext_footnote_definition}) for [ref]
        somewhere in the document, otherwise the reference will not
        parse back. *)

    val text : t -> inline
    (** [text l] is the text of the link. *)

    val reference : t -> reference
    (** [reference l] is the reference of the link. *)

    val referenced_label : t -> Label.t option
    (** [referenced_label l] is the label referenced by the label of [l].
        This is the second label of [`Ref _] or [None] on inline
        references.*)

    val reference_definition : Label.defs -> t -> Label.def option
    (** [reference_definition defs l] is the definition of [l]'s
        reference. If [l] is an [`Inline] reference this returns its
        link definition wrapped in a {!Link_definition.Def}. If [l] is
        [`Ref] this looks up the {!referenced_label} in [defs]. *)

    val is_unsafe : string -> bool
    (** [is_unsafe url] is [true] if [url] is deemed unsafe. This is
        the case if [url] starts with a caseless match of
        [javascript:], [vbscript:], [file:] or [data:] except if
        [data:image/{gif,png,jpeg,webp}]. These rules were taken from
        {{:https://github.com/commonmark/cmark}[cmark]}, the C
        reference implementation of CommonMark and are likely
        incomplete. If you are trying to prevent XSS you should
        post-process rendering outputs with a dedicated HTML sanitizer. *)
  end

  (** Raw HTML. *)
  module Raw_html : sig

    type t = Block_line.tight list
    (** The type for {{:https://spec.commonmark.org/0.31.2/#raw-html}inline raw
        HTML} (can span multiple lines).

        {b Warning.} If you create HTML blocks using
        {!Block_line.tight_list_of_string} you should make sure the
        resulting lines satisfy the contraints of CommonMark raw HTML
        (one way is to parse them instead). *)
  end

  (** Text. *)
  module Text : sig
    type t = string
    (** The type for
        {{:https://spec.commonmark.org/0.31.2/#textual-content}textual content}.

        Normally these strings should not contain newlines. This can
        however happen if the source had newlines as
        {{:https://spec.commonmark.org/0.31.2/#entity-and-numeric-character-references}character references}. *)
  end

  type t +=
  | Autolink of Autolink.t node
  | Break of Break.t node
  | Code_span of Code_span.t node
  | Emphasis of Emphasis.t node
  | Image of Link.t node
  | Inlines of t list node (** Splicing *)
  | Link of Link.t node
  | Raw_html of Raw_html.t node
  | Strong_emphasis of Emphasis.t node
  | Text of Text.t node (** *)
  (** The
      CommonMark {{:https://spec.commonmark.org/0.31.2/#inlines}inlines}. *)

  val empty : t
  (** [empty] is [Inlines ([], Meta.none)]. *)

  (** {1:exts Extensions}

      See the description of {{!Cmarkit.extensions}extensions}. *)

  (** Strikethrough. *)
  module Strikethrough : sig
    type inline := t

    type t
    (** The type for {{!Cmarkit.ext_strikethrough}strikethrough}. *)

    val make : inline -> t
    (** [make i] is [i] with a strikethrough. *)

    val inline : t -> inline
    (** [inline s] is the inline with a strikethrough. *)
  end

  (** Extra inline containers. *)
  module Extra_inline_container : sig
    type inline := t

    type kind = Highlight | Superscript | Subscript | Inserted | Deleted
    (** The kinds of oymarkit-owned extra inline containers. *)

    module Config : sig
      type syntax = Disabled | Curly_required | Curly_optional
      (** The accepted syntax for an extra inline container kind. *)

      type t
      (** Parser configuration for extra inline containers. *)

      val make :
        ?highlight:syntax -> ?superscript:syntax -> ?subscript:syntax ->
        ?inserted:syntax -> ?deleted:syntax -> unit -> t
      (** [make ()] disables every extra inline container. *)

      val disabled : t
      (** [disabled] disables every extra inline container. *)

      val explicit : t
      (** [explicit] enables every extra inline container with compulsory
          curly braces. *)
    end

    type t
    (** The type for extra inline container extensions. *)

    val make : kind -> inline -> t
    (** [make kind i] is an extra inline container of kind [kind] around [i]. *)

    val kind : t -> kind
    (** [kind c] is [c]'s extra inline container kind. *)

    val inline : t -> inline
    (** [inline c] is [c]'s contained inline. *)
  end

  module Attributes : sig
    type inline := t
    type t

    val make : specs:Attribute.t list -> inline -> t
    val inline : t -> inline
    val attributes : t -> Attribute.t
    val specs : t -> Attribute.t list
  end

  (** Math span. *)
  module Math_span : sig
    type t
    (** The type for {{!Cmarkit.ext_math_inline}math spans}. *)

    val make : display:bool -> Block_line.tight list -> t
    (** [make tex_layout] is an inline or display math span with given
        T{_E}X code.  *)

    val display : t -> bool
    (** [display ms] is [true] if the span should be on its own line. *)

    val tex : t -> string
    (** [tex ms] is the inline math T{_E}X code of [ms] *)

    val tex_layout : t -> Block_line.tight list
    (** [tex_layout ms] is inline math T{_E}X code in a form that
        allows layout preservation.

        The acual code data is the tight block lines concatenated and
        separated by space. The {!tex} function does that for you. *)
  end

  [@@@ocamlformat "enable"]

  (** Djot smart punctuation. *)
  module Smart_punct : sig
    type kind =
    | Left_double_quote | Right_double_quote
    | Left_single_quote | Right_single_quote
    | Ellipsis (** [...] *)
    | Em_dash (** [---] *)
    | En_dash (** [--] *)
    (** The type for kinds of
        {{:https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#smart-punctuation}smart punctuation}. *)

    type t
    (** The type for smart punctuation. *)

    val make : ?marker:bool -> kind -> t
    (** [make kind] is smart punctuation of kind [kind]. [marker] indicates a
        quote written with an explicit brace override, [{"] or ["}]; it
        defaults to [false] and is meaningless for the other kinds. *)

    val kind : t -> kind
    (** [kind sp] is the kind of [sp]. *)

    val marker : t -> bool
    (** [marker sp] is [true] if [sp] is a quote whose direction was forced by
        an explicit [{"] or ["}] brace override rather than inferred from
        context. *)

    val to_source : t -> string
    (** [to_source sp] is [sp] in its source form, brace override included.
        Note that a marked quote must keep its braces to survive a roundtrip: a
        bare quote may curl the other way when re-parsed, since direction is
        otherwise inferred from context. *)

    val to_utf_8 : t -> string
    (** [to_utf_8 sp] is the UTF-8 encoded character [sp] denotes, e.g. ["\u{2014}"]
        for {!Em_dash}. *)

    val divide_hyphens : int -> int * int
    (** [divide_hyphens n] is the [(em, en)] count of em- and en-dashes a run of
        [n] hyphens becomes: uniformly if it can, preferring em-dashes when
        either would be uniform. So [4] is two en-dashes and [6] is two
        em-dashes. A run of [1] is [(0, 0)]: a lone hyphen is left alone. The
        counts always account for the whole run, [3 * em + 2 * en = n]. *)
  end

  [@@@ocamlformat "disable"]

  (** Djot symbols. *)
  module Symbol : sig
    type t
    (** The type for {{:https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#symbols}djot symbols}:
        a word surrounded by [:], as in [:smile:].

        A symbol carries no meaning of its own. Djot renders it literally and
        leaves interpretation (mapping it to an emoji, say) to a downstream
        filter; the renderers here do the same. *)

    val make : string -> t
    (** [make name] is a symbol named [name], which is the text between the
        two [:]. *)

    val name : t -> string
    (** [name s] is the name of [s], without the delimiting [:]. *)

    val to_source : t -> string
    (** [to_source s] is [s] in its source form, [:name:]. This is also how
        [s] renders, since a symbol renders literally. *)
  end

  (** Obsidian wikilinks. *)
  module Wikilink : sig
    type fragment =
    | Heading of string list
        (** The ['#']-separated heading path, e.g. [ ["H1"; "H2"] ] for
            [ [[Note#H1#H2]] ]. *)
    | Block_ref of string (** A block reference [id] for [ [[Note#^id]] ]. *)
    (** The type for the part of a wikilink after the first ['#']. *)

    type t
    (** The type for {{!Cmarkit.ext_wikilink}wikilinks}. *)

    val make : embed:bool -> string -> t
    (** [make ~embed content] parses the text [content] found between the
        wikilink brackets (without the brackets or leading ['!']) into a
        wikilink. [embed] is [true] for the [ ![[...]] ] embed form. *)

    val content : t -> string
    (** [content w] is the raw text between the brackets, kept verbatim so
        that rendering roundtrips. *)

    val target : t -> string option
    (** [target w] is the link target (before any ['#'] or ['|']), if any. *)

    val fragment : t -> fragment option
    (** [fragment w] is the heading or block reference, if any. *)

    val display : t -> string option
    (** [display w] is the display text (after ['|']), if any. *)

    val embed : t -> bool
    (** [embed w] is [true] if [w] is an [ ![[...]] ] embed. *)

    val to_commonmark : t -> string
    (** [to_commonmark w] renders [w] back to its [ [[...]] ] (or
        [ ![[...]] ]) source. *)

    val to_plain_text : t -> string
    (** [to_plain_text w] is the display text, or the target, or the raw
        content, in that order of preference. *)
  end

  (** JSX expression containers [ {expr} ]. *)
  module Jsx_expr : sig
    type t
    (** The type for {{!Cmarkit.ext_jsx_expr}JSX expression containers}. *)

    val make : string -> t
    (** [make expr] is a container holding the raw embedded code [expr] found
        between the braces (without the braces). The code is kept verbatim and
        interpreted downstream by the consumer, not parsed here. *)

    val expr : t -> string
    (** [expr j] is the raw embedded code between the braces. *)
  end

  (** JSX elements: self-closing [ <Tag attrs... /> ] and inline containers
      [ <Tag attrs...>inlines</Tag> ] (including fragments [ <>...</> ]). *)
  module Jsx_element : sig

    type inline := t

    type t
    (** The type for {{!Cmarkit.ext_jsx_element}JSX elements}. *)

    val make : string -> t
    (** [make raw] is a self-closing element holding its full verbatim source
        [raw] (from ['<'] to ['>'] inclusive, ending in ["/>"]). The tag and
        attributes are kept verbatim and parsed downstream by the consumer, not
        here. Its {!children} are [None]. *)

    val make_container : string -> inline -> t
    (** [make_container raw children] is an inline container element whose
        opening tag source is [raw] (from ['<'] to its terminating ['>']
        inclusive, e.g. ["<Card a=1>"] or the fragment ["<>"]) and whose parsed
        Markdown inline content is [children]. *)

    val raw : t -> string
    (** [raw e] is the verbatim opening (or self-closing) tag source. *)

    val children : t -> inline option
    (** [children e] is [None] for a self-closing element and [Some i] for a
        container, where [i] is the parsed Markdown inline content between the
        opening and closing tags. *)

    val name : t -> string
    (** [name e] is the tag name in [raw] (e.g. ["Card"], ["Foo.Bar"]), or [""]
        for a fragment. *)

    val close_tag : t -> string
    (** [close_tag e] is the matching closing tag reconstructed from [name],
        e.g. ["</Card>"] or ["</>"]. Used to roundtrip a container. *)
  end

  type t +=
  | Ext_strikethrough of Strikethrough.t node
  | Ext_extra_inline_container of Extra_inline_container.t node
  | Ext_attributes of Attributes.t node
  | Ext_math_span of Math_span.t node
  | Ext_smart_punct of Smart_punct.t node
    (** djot {{!Inline.Smart_punct}smart punctuation} *)
  | Ext_symbol of Symbol.t node (** djot {{!Inline.Symbol}symbol} *)
  | Ext_wikilink of Wikilink.t node
  | Ext_jsx_expr of Jsx_expr.t node
  | Ext_jsx_element of Jsx_element.t node
  (** The supported inline extensions. These inlines are only parsed when
      {!Doc.of_string} is called with [strict:false].

      {!Ext_wikilink}, {!Ext_jsx_expr} and {!Ext_jsx_element} are gated
      separately behind the [wikilink], [jsx_expr] and [jsx_element] arguments
      of {!Doc.of_string}. *)

  (** {1:funs Functions} *)

  val is_empty : t -> bool
  (** [is_empty i] is [true] if [i] is [Inline ([], _)] or [Text ("", _)]. *)

  val meta : ?ext:(t -> Meta.t) -> t -> Meta.t
  (** [meta ~ext i] is the metadata of [i].

      [ext] is called on cases not defined in this module. The default
      raises [Invalid_argument]. *)

  val normalize : ?ext:(t -> t) -> t -> t
  (** [normalize i] has the same content as [i] but is such that for any
      occurence of [Inlines (is, _)] in [i] the list of inlines [is]:
      {ol
      {- [is] is not a singleton list.}
      {- Has no two consecutive [Text _] cases. If that occurs the texts are
       concatenated, the meta of the first one is kept and its text
       location extended to include the second one.}
      {- Has no [Inlines _] case. The meta is dropped and the nested
       inlines are spliced in [is] where the case occurs.}}

      [ext] is called on cases not defined in this module. The default
      raises [Invalid_argument].  *)

  val to_plain_text :
    ?ext:(break_on_soft:bool -> t -> t) -> break_on_soft:bool ->
    t -> string list list
    (** [to_plain_text ~ext ~break_on_soft i] has the plain text of [i]
        as a sequence of lines represented by a list of strings to be
        concatenated. If [break_on_soft] is [true] soft line breaks
        are turned into hard line breaks. To turn the result [r]
        in a single string apply:

        {[ String.concat "\n" (List.map (String.concat "") r) ]}

        [ext] is called on cases not defined in this module, it should
        compile extensions to one of these cases. The default raises
        [Invalid_argument]. *)

  val id : ?buf:Buffer.t -> ?ext:(break_on_soft:bool -> t -> t) -> t -> string
  (** [id ?buf i] derives an identifier for inline [i] using [buf] as
      scratch space (one is created if unspecified).

      This converts [i] to plain text using {!Inline.to_plain_text},
      then applies the same
      {{:https://spec.commonmark.org/0.31.2/#matches}normalization}
      performed on labels, maps spaces to character [-] (U+002D),
      drops {{:https://spec.commonmark.org/0.31.2/#unicode-punctuation-character}
      Unicode punctuation characters} except [-] (U+002D) and [_] ([U+005F]).

      [ext] is given to {!Inline.to_plain_text}. *)
end

(** Blocks. *)
module Block : sig

  (** {1:blocks Blocks} *)

  type t = Cmarkit_.Block.t = ..
  (** The type for blocks. *)

  (** Blank lines. *)
  module Blank_line : sig
    type t = Layout.blanks
    (** The type for
        {{:https://spec.commonmark.org/0.31.2/#blank-lines}blank lines}.
        These can be ignored during rendering, they are kept for layout. *)
  end

  (** Block quotes. *)
  module Block_quote : sig

    type block := t

    type t
    (** The type for {{:https://spec.commonmark.org/0.31.2/#block-quotes}
        block quotes}. *)

    val make : ?indent:Layout.indent -> block -> t
    (** [make b] quotes block [b]. *)

    val indent : t -> Layout.indent
    (** [indent bq] is the indentation to the block quote
        marker found on the first line. *)

    val block : t -> block
    (** [block bq] is the quoted block. *)
  end

  (** Code blocks. *)
  module Code_block : sig

    type fenced_layout =
      { indent : Layout.indent; (** Indent to opening fence *)
        opening_fence : Layout.string node;
        (** Opening fence (before info string). *)
        closing_fence : Layout.string node option;
        (** Closing fence (if any). *) }
    (** The type for fenced code block layouts. *)

    type layout = [ `Indented | `Fenced of fenced_layout ]
    (** The type for code block layouts. *)

    type t
    (** The type for
        {{:https://spec.commonmark.org/0.31.2/#indented-code-block}
        indented} and
        {{:https://spec.commonmark.org/0.31.2/#fenced-code-blocks}fenced}
        code blocks. *)

    val make :
      ?layout:layout -> ?info_string:string node -> Block_line.t list -> t
    (** [make ?layout ?info_string code] is a code block with given
        parameters. [layout] defaults to a fenced layout. If [layout]
        is [`Indented] and an [info_string] is provided, the layout is
        switched to [`Fenced]. *)

    val layout : t -> layout
    (** [layout cb] is the layout of [cb]. *)

    val info_string : t -> string node option
    (** [info_string cb] is the
        {{:https://spec.commonmark.org/0.31.2/#info-string}info string}
        of [cb], if any. *)

    val code : t -> Block_line.t list
    (** [code cb] are the code lines of [cb]. *)

    val make_fence : t -> Layout.char * Layout.count
    (** [make_fence cb] is a fence character and count suitable for [cb]. *)

    val language_of_info_string : string -> (string * string) option
    (** [language_of_info_string s] extracts a (non-empty) language,
        the first word of [s] and a trimmed remainder. Assumes [s] is
        {!String.trim}ed which is what {!info_string} gives you. *)
  end

  (** Headings. *)
  module Heading : sig

    type atx_layout =
      { indent : Layout.indent; (** Indent to ['#']. *)
        after_opening : Layout.blanks; (** Blanks after ['#']. *)
        closing : Layout.string; (** Closing sequence of ['#'] and blanks. *) }
    (** The type for ATX heading layout. *)

    type setext_layout =
      { leading_indent : Layout.indent; (** Of heading first line. *)
        trailing_blanks : Layout.blanks; (** Of heading last line. *)
        underline_indent : Layout.indent; (** Indentation of underline. *)
        underline_count : Layout.count node; (** Underline char count. *)
        underline_blanks : Layout.blanks; (** Underline trailing blanks. *) }
    (** The type for setext heading layout. *)

    type layout = [ `Atx of atx_layout | `Setext of setext_layout ]
    (** The type for heading layouts. *)

    type id =
    [ `Auto of string (** Automatically derived. *)
    | `Id of string (** Explicitely specified in another way. *) ]
    (** The type for heading identifiers. This notion does not
        exist in CommonMark. *)

    type t
    (** The type for {{:https://spec.commonmark.org/0.31.2/#atx-headings}
        ATX} and {{:https://spec.commonmark.org/0.31.2/#setext-headings}Setext}
        headings. *)

    val make : ?id:id -> ?layout:layout -> level:int -> Inline.t -> t
    (** [make ~level text] is a heading with given
        parameters. [layout] defaults to [`Atx] so you should make
        sure [text] has no breaks. [level] is clamped to 1-6 or 1-2
        depending on [layout]. [id] is an identifier for the heading. *)

    val layout : t -> layout
    (** [layout h] is the layout of [h]. *)

    val level : t -> int
    (** [level h] is the level of [h], from [1] to [6]. *)

    val inline : t -> Inline.t
    (** [inline h] is the contents of the heading. *)

    val id : t -> id option
    (** [id h] is the heading identifier (if any). Can be automatically
        derived at parse time from the heading {!inline} if
        {{!Doc.of_string}[heading_auto_ids:true]}.
        Can also be derived later via {!Inline.id}. *)
  end

  (** HTML blocks. *)
  module Html_block : sig
    type t = Block_line.t list
    (** The type for {{:https://spec.commonmark.org/0.31.2/#html-blocks}HTML
        blocks}.

        {b Warning.} If you create HTML blocks using
        {!Block_line.list_of_string} you should make sure the resulting
        lines satisfy the contraints of CommonMark HTML blocks.
        (one way is to parse them instead). *)
  end

  (** List items. *)
  module List_item : sig

    type block := t

    type t
    (** The type for {{:https://spec.commonmark.org/0.31.2/#list-items}list
        items}. *)

    val make :
      ?before_marker:Layout.indent -> ?marker:Layout.string node ->
      ?after_marker:Layout.indent -> ?ext_task_marker:Uchar.t node ->
      block -> t
    (** [make b] is a list item for block [b] with given parameters, see
        corresponding accessors for semantics. *)

    val block : t -> block
    (** [block i] is the contents of [i]. *)

    val before_marker : t -> Layout.indent
    (** [before_marker i] is the indentation before the list marker. *)

    val marker : t -> Layout.string node
    (** [marker i] is the item marker layout of [i]. *)

    val after_marker : t -> Layout.indent
    (** [after_marker i] is the indentation after the marker. *)

    (** {1:ext_task_item Task items} *)

    val ext_task_marker : t -> Uchar.t node option
    (** [ext_task_marker i] is a task marker, only occurs in non-strict
        parsing mode, see see extension
        {{!Cmarkit.ext_list_task_items}list task items}.*)

    val task_status_of_task_marker :
      Uchar.t -> [ `Unchecked | `Checked | `Cancelled | `Other of Uchar.t ]
    (** [task_status_of_task_marker u] is a status for marker [u], see extension
        {{!ext_list_task_items}list task item}. *)
  end

  (** Lists. *)
  module List' : sig

    type type' =
    [ `Unordered of Layout.char (** with given marker. *)
    | `Ordered of int * Layout.char
      (** starting at given integer, markers ending with given character
          ([')'] or ['.']). *) ]
    (** The type for list types. *)

    type t
    (** The type for {{:https://spec.commonmark.org/0.31.2/#lists}lists}. *)

    val make : ?tight:bool -> type' -> List_item.t node list -> t
    (** [make ?tight t items] is a list with given parameters.
        tight default to [true], but should be computed from [items]
        in practice. *)

    val type' : t -> type'
    (** [type' l] is the list type of [l]. *)

    val tight : t -> bool
    (** [tight l] is [true] iff the list is
        {{:https://spec.commonmark.org/0.31.2/#tight}tight}. *)

    val items : t -> List_item.t node list
    (** [items l] are the items of [l]. *)
  end

  (** Obsidian-style block identifiers. *)
  module Block_id : sig
    type t
    (** The type for a block identifier parsed from a terminal caret suffix
        such as [^block-id]. *)

    val id : t -> string
    (** [id t] is the identifier without the caret marker. *)

    val marker : t -> Meta.t
    (** [marker t] is the metadata for the caret marker and identifier. *)

    val find : Meta.t -> t option
    (** [find meta] is the block identifier attached to [meta], if any. *)
  end

  (** Obsidian callouts.

      A callout is a {!Block_quote} whose first line is a
      [ [!kind](+|-)? title] header. Callouts are not a distinct block: the
      node stays a {!Block_quote} and the parsed metadata is attached to its
      {!Meta.t}, found with {!Callout.find}. The header line is kept in the
      block-quote body (so CommonMark rendering roundtrips it verbatim);
      renderers drop it with {!Callout.strip_header}. Enabled by the [callout]
      argument of {!Doc.of_string}. *)
  module Callout : sig
    type block := t

    type fold = Foldable_open | Foldable_closed
    (** The initial state of a foldable callout: expanded ([+]) or
        collapsed ([-]). *)

    (** Parser configuration for callouts. *)
    module Config : sig
      type kinds = Any | Only of string list
      (** Which [ [!kind]] identifiers are recognised as callouts: [Any]
          identifier, or [Only] a given list of lowercased kinds (others stay
          plain blockquotes). *)

      type t
      (** The type for callout parser configuration. *)

      val make : ?kinds:kinds -> unit -> t
      (** [make ()] enables callouts, recognising [Any] kind by default. *)

      val disabled : t
      (** [disabled] disables callout parsing. *)
    end

    type t
    (** The type for callout metadata attached to a {!Block_quote}'s
        {!Meta.t}. It carries only the decoded {!kind} and {!fold}; the title
        is not stored — it lives in the block-quote body and is recovered with
        {!title}. *)

    val make : ?fold:fold -> string -> t
    (** [make ~fold kind] is a callout of the given lowercased [kind]. *)

    val kind : t -> string
    (** [kind c] is the lowercased type identifier, e.g. ["info"], ["tip"]. *)

    val fold : t -> fold option
    (** [fold c] is the fold state, or [None] if the callout is not
        foldable. *)

    val find : Meta.t -> t option
    (** [find meta] is the callout attached to [meta], if any. *)

    val add : t -> Meta.t -> Meta.t
    (** [add c meta] attaches callout [c] to [meta]. *)

    val title : t -> block -> Inline.t option
    (** [title c inner] is the callout title as inline content: the header line
        of the block-quote content [inner] with the [ [!kind](+|-)?] prefix and
        following blanks removed. [None] selects the default (the capitalised
        {!kind}). Inline formatting, links and wikilinks are preserved; the
        title shares nodes with [inner], so mappers transform both at once. *)

    val strip_header : block -> block
    (** [strip_header inner] is the body of a callout: the block-quote content
        [inner] with its first line (the [ [!kind]…] header) removed. A
        structural rewrite that neither re-parses nor needs text locations. *)
  end

  (** Paragraphs. *)
  module Paragraph : sig

    type t
    (** The type for
          {{:https://spec.commonmark.org/0.31.2/#paragraphs}paragraphs}. *)

    val make :
      ?leading_indent:Layout.indent -> ?trailing_blanks:Layout.blanks ->
      Inline.t -> t
      (** [make inline] is a paragraph with given parameters. *)

    val inline : t -> Inline.t
    (** [inline p] is the paragraph content. *)

    val leading_indent : t -> Layout.indent
    (** [leading_indent p] is the indent on the first line (0-3). *)

    val trailing_blanks : t -> Layout.blanks
    (** [trailing_blanks] are trailing blanks on the last line. *)
  end

  module Attributes : sig
    type block := t
    type t

    val make : specs:Attribute.t list -> block -> t
    val block : t -> block
    val attributes : t -> Attribute.t
    val specs : t -> Attribute.t list
  end

  (** Thematic breaks. *)
  module Thematic_break : sig

    type t
    (** The type for {{:https://spec.commonmark.org/0.31.2/#thematic-break}
        thematic breaks}. *)

    val make : ?indent:Layout.indent -> ?layout:Layout.string -> unit -> t
    (** [make ()] is a thematic break with given parameters. [layout]
        defaults to ["---"]. *)

    val indent : t -> Layout.indent
    (** [indent t] is the thematic break indent (0-3). *)

    val layout : t -> Layout.string
    (** [layout t] is the thematic break art, including trailing blanks. *)
  end

  type t +=
  | Blank_line of Blank_line.t node
  | Block_quote of Block_quote.t node
  | Blocks of t list node (** Splicing *)
  | Code_block of Code_block.t node
  | Heading of Heading.t node
  | Html_block of Html_block.t node
  | Link_reference_definition of Link_definition.t node
    (** {{:https://spec.commonmark.org/0.31.2/#link-reference-definitions}
        Link reference definitions}, kept for layout *)
  | List of List'.t node
  | Paragraph of Paragraph.t node
  | Ext_attributes of Attributes.t node
  | Thematic_break of Thematic_break.t node
    (** {{:https://spec.commonmark.org/0.31.2/#paragraphs}Thematic break} *)
  (** The CommonMark {{:https://spec.commonmark.org/0.31.2/#leaf-blocks}leaf}
      and {{:https://spec.commonmark.org/0.31.2/#container-blocks}container}
      blocks. *)

  val empty : t
  (** [empty] is [Blocks ([], Meta.none)]. *)

  (** {1:exts Extensions}

      See the description of {{!Cmarkit.extensions}extensions}. *)

  (** Tables. *)
  module Table : sig

    type align = [ `Left | `Center | `Right ]
    (** The type for column alignments. *)

    type sep = align option * Layout.count
    (** The type for separators. The column aligment and the number of
        [-] for layout preservation. *)

    type cell_layout = Layout.blanks * Layout.blanks
    (** The type for cell layout, initial and trailing blanks. *)

    type row =
    [ `Header of (Inline.t * cell_layout) list
    | `Sep of sep node list
    | `Data of (Inline.t * cell_layout) list ]
    (** The type for rows. The lists only have entries for columns as
        found in rows in the document. You need to pad them on the
        right with more columns to reach the table's {!col_count}. *)

    type t
    (** The type for {{!Cmarkit.ext_tables}tables}. *)

    val make : ?indent:Layout.indent -> (row node * Layout.blanks) list -> t
    (** [make rows] is a table row [rows]. *)

    val indent : t -> Layout.indent
    (** [indent t] is the indentation to the first pipe found on the
        first row. *)

    val col_count : t -> int
    (** [col_count t] is the number of columns in the table. *)

    val rows : t -> (row node * Layout.blanks) list
    (** [rows t] are the table's rows. *)
  end

  (** Footnotes. *)
  module Footnote : sig

    (** {1:footnotes Footnotes} *)

    type block := t

    type t
    (** The type for {{!Cmarkit.ext_footnotes}footnotes}. *)

    val make :
      ?indent:Layout.indent -> ?defined_label:Label.t option -> Label.t ->
      block -> t
    (** [make label b] is a footnote for label [label] with content [b].
        [defined_label] defaults to [label]. *)

    val indent : t -> Layout.indent
    (** [indent fn] is the indentation to the label found on the first line. *)

    val label : t -> Label.t
    (** [label fn] is the footnote definition label as found in the
        source text. It includes the [^]. See also {!defined_label}. *)

    val defined_label : t -> Label.t option
    (** [defined_label fn] is the label determined by the {!Label.type-resolver}
        for the footnote. The label as found in the source text is in {!label}.
        If this is [None] the resolver deleted the label definition. *)

    val block : t -> block
    (** [block fn] is the footnote content. *)

    (** {1:labeldef As label definitions} *)

    type Label.def += Def of t node (** *)
    (** A label definition for footnotes. *)
  end

  (** Djot divs. *)
  module Div : sig

    type block := t

    type t
    (** The type for {{:https://djot.net/}djot} divs: a fenced container of
        block-level content with an optional class name. Only parsed when
        {!Doc.of_string} is called with [div:true]. *)

    val make :
      ?indent:Layout.indent -> ?opening_fence:Layout.string node ->
      ?class':string node -> ?closing_fence:Layout.string node option ->
      block -> t
    (** [make b] is a div with content [b]. *)

    val indent : t -> Layout.indent
    (** [indent d] is the indentation to the opening fence. *)

    val opening_fence : t -> Layout.string node
    (** [opening_fence d] is the opening colon fence (excluding the class). *)

    val class' : t -> string node option
    (** [class' d] is the div's class name, if any. *)

    val closing_fence : t -> Layout.string node option
    (** [closing_fence d] is the closing colon fence, or [None] if the div
        was closed by the end of the document or its containing block. *)

    val block : t -> block
    (** [block d] is the div's content. *)
  end

  (** JSX container elements at block level:
      [ <Card ...> ] … Markdown blocks … [ </Card> ], including fragments. *)
  module Jsx_block : sig

    type block := t

    type t
    (** The type for {{!Cmarkit.ext_jsx_block}block-level JSX containers}. Only
        parsed when {!Doc.of_string} is called with [jsx_element:true]. *)

    val make :
      ?indent:Layout.indent -> raw_open:string node ->
      ?raw_close:string node -> block -> t
    (** [make ~raw_open b] is a JSX container with opening tag source [raw_open]
        (e.g. ["<Card a=1>"] or the fragment ["<>"]), block content [b] and
        optional closing tag [raw_close] (e.g. ["</Card>"]). *)

    val indent : t -> Layout.indent
    (** [indent j] is the indentation to the opening tag. *)

    val raw_open : t -> string node
    (** [raw_open j] is the verbatim opening tag source. Attributes stay raw;
        downstream re-parses them (positions recoverable by offset). *)

    val block : t -> block
    (** [block j] is the container's parsed block content. *)

    val raw_close : t -> string node option
    (** [raw_close j] is the verbatim closing tag, or [None] if the container
        was closed by the end of the document or its containing block. *)
  end

  type t +=
  | Ext_math_block of Code_block.t node
    (** {{!Cmarkit.ext_math_display}display math}*)
  | Ext_table of Table.t node (** *)
  | Ext_footnote_definition of Footnote.t node (** *)
  | Ext_div of Div.t node (** djot {{!Block.Div}div} *)
  | Ext_jsx_block of Jsx_block.t node (** block-level {{!Block.Jsx_block}JSX container} *)
  | Ext_keyed of (Inline.t * t) node
    (** a colon-keyed node produced by the {!Cmarkit.Struct} pass: the
        {!Inline.t} is the label, the [t] the keyed body. Whether it renders
        with a list marker is decided by context -- a keyed node that is a
        {!List_item} block renders as a list item, otherwise as a
        paragraph-level block. *)
  (** The supported block extensions. These blocks are only parsed when
      {!Doc.of_string} is called with [strict:false]. The {!Ext_keyed}
      nodes are produced by the {!Cmarkit.Struct} pass rather than the
      parser. *)

  (** {1:funs Functions on blocks} *)

  val meta : ?ext:(t -> Meta.t) -> t -> Meta.t
  (** [meta ~ext b] is the metadata of [b].

      [ext] is called on cases not defined in this module. The default
      raies [Invalid_argument]. *)

  val normalize : ?ext:(t -> t) -> t -> t
  (** [normalize b] has the same content as [b] but is such that for any
      occurence of [Blocks (bs, _)] in [b] the list of blocks [bs]:

      {ol
      {- [bs] is not a singleton list.}
      {- Has no [Blocks _] case. The meta is dropped and the nested
         blocks are spliced in [bs] where the case occurs.}}

      [ext] is called on cases not defined in this module. The default raises
      [Invalid_argument]. *)

  val defs :
    ?ext:(Label.defs -> t -> Label.defs) -> ?init:Label.defs -> t ->
    Label.defs
  (** [defs b] collects [b]'s {!Link_reference_definition} and
      {!Ext_footnote_definition} and for those that have a label
      definition (see {!Link_definition.defined_label} and
      {!Footnote.defined_label})
      adds them to [init] (defaults to {!Label.Map.empty}).

      [ext] is called on cases not defined in this module. The default
      raises [Invalid_argument]. *)
end

(** Documents (and parser). *)
module Doc : sig

  (** {1:docs Documents} *)

  type t = Cmarkit_.Doc.t
  (** The type for CommonMark documents. *)

  val nl : t -> Layout.string
  (** [nl d] is the first newline found in the text during parsing
      or ["\n"] if there was none. *)

  val block : t -> Block.t
  (** [block d] is the document's contents as a block. *)

  val defs : t -> Label.defs
  (** [defs d] are the label definitions resulting from parsing [d].
      The result depends on the label definitions found in the
      source and the [defs] and [resolver] values specified on
      {!Doc.of_string}. *)

  val make : ?nl:Layout.string -> ?defs:Label.defs -> Block.t -> t
  (** [make ~nl ~defs b] is a document for block [b] with newline
      [nl] (defaults to ["\n"]), label definition [defs]
      (defaults to {!Label.Map.empty}). *)

  val empty : t
  (** [empty] is an empty document. *)

  (** {1:parsing Parsing} *)

  val of_string :
    ?defs:Label.defs -> ?resolver:Label.resolver -> ?nested_links:bool ->
    ?heading_auto_ids:bool -> ?layout:bool -> ?locs:bool ->
    ?file:Textloc.fpath -> ?emphasis_delims:char list ->
    ?strong_emphasis_delims:char list -> ?intraword_emphasis:bool ->
    ?marked_emphasis_delims:bool -> ?strong_emphasis_width:int ->
    ?extra_inline_containers:Inline.Extra_inline_container.Config.t ->
    ?block_id:bool -> ?djot_inline_attributes:bool ->
    ?djot_block_attributes:bool -> ?djot_thematic_break:bool ->
    ?djot_symbols:bool -> ?smart_punctuation:bool ->
    ?indented_code:bool -> ?setext_headings:bool ->
    ?div:bool -> ?wikilink:bool ->
    ?jsx_expr:bool -> ?jsx_element:bool ->
    ?callout:Block.Callout.Config.t ->
    ?strict:bool -> string -> t
    (** [of_string md] is a document from the UTF-8 encoded CommonMark
        document [md].

    {ul
    {- If [strict] is [true] (default) the CommonMark specification is
       followed. If [false] these {{!extensions}extensions} are enabled.}
    {- [file] is the file path from which [s] is assumed to have been read
       (defaults to {!Textloc.file_none}), used in the {!Textloc.t}
       values iff [locs] is [true].}
    {- If [locs] is [true] locations are stored in nodes of the abstract
       syntax tree in individually {{!Meta.val-id}identified} {!Meta.t}
       values. If [false] (default) node meta values are all {!Meta.none}
       whose text location is {!Textloc.none}.}
    {- If [layout] is [false] (default) layout values cannot be relied
       upon and do not in general represent source layout, some fields
       may be normalized. The {!Block.extension-Blank_line},
        {!Block.extension-Link_reference_definition},
        {!Block.extension-Ext_footnote_definition},
        layout block values are present in the result regardless of this
        parameter.}
    {- If [heading_auto_ids] is [true] (defaults to [false]) then [`Auto]
       {{!Block.Heading.type-id}heading identifiers} are generated
       during parsing from the header text
       with {!Inline.id} (at that point no [ext] argument is needed)
       and made accessible in {!Block.Heading.val-id}. Note that the identifiers
       may not be unique, we leave it to the backends to handle this
       problem.}
   {- If [nested_links] is [true] (defaults to [false]) there is no
       restriction on having links in link text, which is forbidden by
       CommonMark and HTML. This can be useful for
       embedding DSLs in link labels or destinations. Note that image
       links already allow link nesting as per CommonMark
       specification.}
   {- [emphasis_delims] and [strong_emphasis_delims] specify the characters
       allowed to delimit emphasis and strong emphasis respectively. Each list
       must be non-empty and contain only ['*'] or ['_']. Both default to
       [['*'; '_']], which preserves CommonMark behavior. If a matching
       delimiter run could form strong emphasis but its character is not in
       [strong_emphasis_delims], the parser may still consume one delimiter
       from each side to form emphasis when the character is in
       [emphasis_delims]. For example, with [emphasis_delims = ['_']] and
       [strong_emphasis_delims = ['*']], [__x__] parses as nested emphasis
       rather than strong emphasis.}
   {- If [intraword_emphasis] is [false], emphasis delimiter runs between two
       non-whitespace, non-punctuation characters cannot open or close
       emphasis. The default is [true], which preserves CommonMark behavior.}
   {- If [marked_emphasis_delims] is [true], [{*] and [{_] mark emphasis
       delimiters as opener-only, and [*}] and [_}] mark them as closer-only.
       A marker forces its role: the delimiter opens (resp. closes) regardless
       of flanking and of [intraword_emphasis], since those rules only serve to
       disambiguate bare runs and the marker has already resolved the role. For
       example [a{_b_] emphasises [b] even though a bare [_] could not open
       there. The default is [false], which preserves CommonMark behavior.}
   {- [strong_emphasis_width] specifies how many delimiter characters are
      consumed from each side to form strong emphasis. It must be [1] or [2].
      The default is [2], which preserves CommonMark behavior.}
   {- [extra_inline_containers] configures which oymarkit extra inline
      containers are parsed and whether curly braces are compulsory for each
      kind. The default is {!Inline.Extra_inline_container.Config.disabled},
      which preserves CommonMark behavior.}
   {- If [block_id] is [true], a terminal suffix of the form
      [^id], where [id] starts with an ASCII alphanumeric character and
      otherwise contains only ASCII alphanumeric characters or hyphens, is
      attached to the paragraph metadata as a {!Block.Block_id.t}. The marker
      remains part of the paragraph inline content. The default is [false],
      which preserves CommonMark behavior.}
   {- If [djot_inline_attributes] is [true], Djot [{...}] attribute
      specifiers immediately following inline content are represented by
      {!Inline.Ext_attributes}.}
   {- If [djot_block_attributes] is [true], Djot attribute lines immediately
      preceding a block are represented by {!Block.Ext_attributes}. Continued
      lines inside a block attribute must be indented.}
   {- If [djot_thematic_break] is [true], a
      {{:https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#thematic-break}djot}
      thematic break is a line of three or more [*] or [-], and nothing else
      but spaces or tabs. Unlike CommonMark, [_] is not a break character:
      [___] is ordinary text. Djot also allows a break to be indented
      arbitrarily; that follows from [indented_code] rather than from this
      knob, since a deep indent is only claimed by something else when
      indented code blocks exist. The default is [false], which preserves
      CommonMark behavior.}
   {- If [djot_symbols] is [true], [:name:] is represented by
      {!Inline.Ext_symbol}, where [name] is a non-empty run of ASCII
      alphanumerics, ['_'], ['+'] or ['-']. An unterminated run stays text.
      Like djot, the renderers emit a symbol literally: giving [:smile:] a
      meaning is a consumer's job. The default is [false], which preserves
      CommonMark behavior.}
   {- If [smart_punctuation] is [true], straight quotes, [...], [--] and [---]
      are represented by {!Inline.Ext_smart_punct}. A quote curls according to
      context: it closes when it is right-flanking, which is also what turns
      the apostrophes of [don't] and [Socrates'] the right way. Braces override
      the inference, [{"] forcing an opener and ["}] a closer, and a
      backslash-escaped quote stays straight. The default is [false], which
      preserves CommonMark behavior.}
   {- If [indented_code] is [false], a line indented by four or more columns no
      longer opens an
      {{:https://spec.commonmark.org/0.31.2/#indented-code-blocks}indented code
      block}; it is dispatched like any other line. Fenced code blocks are
      unaffected. Djot has no indented code blocks. The default is [true],
      which preserves CommonMark behavior.}
   {- If [setext_headings] is [false],
      {{:https://spec.commonmark.org/0.31.2/#setext-headings}setext heading}
      underlines are not recognized, so [---] under a paragraph is free to be
      read as a thematic break instead of an [<h2>]. Djot has no setext
      headings. The default is [true], which preserves CommonMark behavior.}
   {- If [wikilink] is [true], Obsidian {{!ext_wikilink}wikilinks} [ [[...]] ]
      and embeds [ ![[...]] ] are represented by {!Inline.Ext_wikilink}. The
      default is [false]. This knob is independent of [strict].}
   {- [callout] enables Obsidian {{!Block.Callout}callouts}: blockquotes whose
      first line is a [ [!kind]] header gain {!Block.Callout.t} metadata.
      Defaults to {!Block.Callout.Config.disabled}. Independent of [strict].}
   {- If [resolver] is provided this is used resolve label definitions
      and references. See {{!Label.resolvers}here} for details. Defaults to
      {!Label.default_resolver}.}
    {- If [defs] adds these label definitions to the document
        (defaults to {!Label.Map.empty}). Think of them
        as being prepended to [md]. If [resolver] is
        {!Label.default_resolver}, these take over the same labels
        defined in [md] (first definition takes over in CommonMark).}}

    UTF-8 decoding errors and U+0000 are turned into {!Uchar.rep}
    characters. Inlines of the result should be {!Inline.normalize}d.
    Blocks of the result should be {!Block.normalize}d.

    {b Note.} For simple renders parsing with [layout:false] and
    [locs:false] is generally faster; having these to [true]
    allocates quite a bit. *)

  (** {1:versions Versions} *)

  val unicode_version : string
  (** [unicode_version] is the Unicode version known to {!of_string}. *)

  val commonmark_version : string
  (** [commonmark_version] is the CommonMark version known to {!of_string}. *)
end

(** {1:maps_and_folds Maps and folds} *)

(** Abstract syntax tree mappers.

    Mappers help with pushing abstract syntax tree transformations in every
    node with a minimal amount of code by defaulting the cases you
    don't handle. The default map maps leaves to themselves and
    otherwise propagates the map to all childrens.

    This map has the form of {!List.filter_map}, however it is akin
    to {!List.concat_map} as it allows:

    {ul
    {- Node deletion by mapping to [None]}
    {- Node transformation by mapping to [Some _]}
    {- Node expansion by mapping to [Some (Inlines _)] or [Some (Blocks _)]}}

    See an {{!Mapper.example}example}. *)
module Mapper : sig

  (** {1:results Map results} *)

  type 'a filter_map = 'a option
  (** The type for maps. [None] is for node deletion. [Some n] is a map
      to [n]. *)

  type 'a result =
  [ `Default (** Do the default map. *) | `Map of 'a filter_map ]
  (** The type for mapper results. *)

  val default : 'a result
  (** [default] is [`Default]. *)

  val delete : 'a result
  (** [delete] is [`Map None]. *)

  val ret : 'a -> 'a result
  (** [ret v] is [`Map (Some v)]. *)

  (** {1:mappers Mappers} *)

  type t
  (** The type for abstract syntax tree mappers. *)

  type 'a map = t -> 'a -> 'a filter_map
  (** The type for maps on values of type ['a]. *)

  type 'a mapper = t -> 'a -> 'a result
  (** The type for mappers on values of type ['a].

      This is what you specify. Return [`Default] if you are not
      interested in handling the given case. Use {!map_inline} or
      {!map_block} with the given mapper if you need to call the
      mapper recursively. *)

  val make :
    ?inline_ext_default:Inline.t map -> ?block_ext_default:Block.t map ->
    ?inline:Inline.t mapper -> ?block:Block.t mapper -> unit -> t
  (** [make ?inline ?block ()] is a mapper using [inline] and [block]
      to map the abstract syntax tree. Both default to [fun _ _ -> `Default].

      The mapper knows how to default the built-in abstract syntax
      tree and the built-in {{!extensions}extensions}. It maps
      them in document and depth-first order.

      If you extend the abstract syntax tree you need to indicate how to default
      these new cases by providing [inline_ext_default] or
      [block_ext_default] functions. By default these functions raise
      [Invalid_argument]. *)

  (** {1:mapping Mapping} *)

  val map_inline : Inline.t map
  (** [map_inline m i] maps [i] with [m]. *)

  val map_block : Block.t map
  (** [map_block m b] maps [b] with [m]. *)

  val map_doc : t -> Doc.t -> Doc.t
  (** [map_doc m d] maps [Doc.block d] with [m]. If the document
      block maps to [None] is replaced by {!Block.empty}.

      {b Warning unstable.} The following may change in the future.
      This function also maps the blocks present {!Block.Footnote.Def}
      label definitions but will not map inline or block data in
      {!Label.def} cases unknown to [Cmarkit]. If the block maps to
      [None] for the footnote it is replaced by {!Block.empty}.

      Also note that if these label definitions were defined in [d]'s
      abstract syntax tree, they will also already be
      mapped in {!Block.Link_reference_definition} and
      {!Block.Ext_footnote_definition} cases. It is possible to collect
      these mapped definitions via {!Block.defs} on the resulting
      document's block. *)

  (** {1:accessors Accessors} *)

  val inline_mapper : t -> Inline.t mapper
  (** [inline m] is the inline mapper of [m]. *)

  val block_mapper : t -> Block.t mapper
  (** [block m] is the block mapper of [m]. *)

  val inline_ext_default : t -> Inline.t map
  (** [inline_ext_default m] is the inline extensions defaulter of [m] *)

  val block_ext_default : t -> Block.t map
  (** [block_ext_default m] is the block extensions defaulter of [m]. *)

  (** {1:example Example}

      This example sets all code blocks of document [doc] without info string
      to [lang].
{[
let set_unknown_code_block_lang ~lang doc =
  let open Cmarkit in
  let default = lang, Meta.none in
  let block m = function
  | Block.Code_block (cb, meta)
    when Option.is_none (Block.Code_block.info_string cb) ->
      let layout = Block.Code_block.layout cb in
      let code = Block.Code_block.code cb in
      let cb = Block.Code_block.make ~layout ~info_string:default code in
      Mapper.ret (Block.Code_block (cb, meta))
  | _ ->
      Mapper.default (* let the mapper thread the map *)
  in
  let mapper = Mapper.make ~block () in
  Mapper.map_doc mapper doc
]}
  *)
end

(** Abstract syntax tree folders.

    Folders help with pushing abstract syntax tree folds in every node
    with a minimal amount of code by defaulting the cases you don't handle.
    The default fold returns the accumulator unchanged on leaves and otherwise
    propagates the fold to all children.

    See an {{!Folder.example}example}. *)
module Folder : sig

  (** {1:results Fold results} *)

  type 'a result = [ `Default (** Do the default fold *) | `Fold of 'a ]
  (** The type for folder results. The [`Default] case indicates the folder
      to perform the default fold. *)

  val default : 'a result
  (** [default] is [`Default]. *)

  val ret : 'a -> 'a result
  (** [ret v] is [`Fold v]. *)

  (** {1:folders Folders} *)

  type 'a t
  (** The type for abstract syntax tree folders with result values of type
      ['a]. *)

  type ('a, 'b) fold = 'b t -> 'b -> 'a -> 'b
  (** The type for folds on values of type ['a]. *)

  type ('a, 'b) folder = 'b t -> 'b -> 'a -> 'b result
  (** The type for folders on value of type ['a].

      This is what you specify. Return [`Default] if you are not
      interested in handling the given case. Use {!fold_inline} or
      {!fold_block} with the given folder if you need to call the
      folder recursively. *)

  val make :
    ?inline_ext_default:(Inline.t, 'a) fold ->
    ?block_ext_default:(Block.t, 'a) fold ->
    ?inline:(Inline.t, 'a) folder -> ?block:(Block.t, 'a) folder -> unit -> 'a t
  (** [make ?inline ?block ()] is a folder using [inline] and [block]
      to fold the abstract syntax tree. Both default to
      [fun _ _ _ -> `Default].

      The folder knows how to default the built-in abstract syntax tree
      and the built-in {{!extensions}extensions}. It folds
      them in document and depth-first order.

      If you extend the abstract syntax tree you need to indicate how
      to default these new cases by providing [inline_ext_default] or
      [block_ext_default] functions. By default these functions raise
      [Invalid_argument]. *)

  (** {1:folding Folding} *)

  val fold_inline : 'a t -> 'a -> Inline.t -> 'a
  (** [fold_inline f acc i] folds [i] with [f] starting with [acc]. *)

  val fold_block : 'a t -> 'a -> Block.t -> 'a
  (** [fold_block f acc b] folds [b] with [f] starting with [acc]. *)

  val fold_doc : 'a t -> 'a -> Doc.t -> 'a
  (** [fold_doc f acc d] folds [Doc.block d] with [f] starting with [acc].

      {b Warning.} Blocks present in [d]'s {!Doc.defs} is not folded
      over. Note however that if these definitions were defined by
      [d]'s abstract syntax tree, they will already have been folded
      over on {!Block.Link_reference_definition} and
      {!Block.Ext_footnote_definition} cases. *)

  (** {1:acesssors Accessors} *)

  val inline_folder : 'a t -> (Inline.t, 'a) folder
  (** [inline_folder f] is the inline folder of [f]. *)

  val block_folder : 'a t -> (Block.t, 'a) folder
  (** [block_folder f] is the block folder of [f]. *)

  val inline_ext_default : 'a t -> (Inline.t, 'a) fold
  (** [inline_ext_default f] is the inline extension defaulter of [f]. *)

  val block_ext_default : 'a t -> (Block.t, 'a) fold
  (** [block_ext_default f] is the block extension defaulter of [f]. *)

  (** {1:example Example}

      This example collects the languages present in the code blocks
      of a document.
{[
let code_block_langs doc =
  let open Cmarkit in
  let module String_set = Set.Make (String) in
  let block m acc = function
  | Block.Code_block (cb, _) ->
      let acc = match Block.Code_block.info_string cb with
      | None -> acc
      | Some (info, _) ->
          match Block.Code_block.language_of_info_string info with
          | None -> acc
          | Some (lang, _) -> String_set.add lang acc
      in
      Folder.ret acc
  | _ ->
      Folder.default (* let the folder thread the fold *)
  in
  let folder = Folder.make ~block () in
  let langs = Folder.fold_doc folder String_set.empty doc in
  String_set.elements langs
]} *)
end

(** {1:pp Debug pretty-printing} *)

(** Structural pretty-printers for the AST.

    These drop {!Meta.t} and most layout, and truncate inline previews.
    Intended for debugging and as a coarse equality (compare the printed
    forms). Not a faithful round-trip. *)
module Pp : sig
  val pp_block_with :
    ?ext:(Format.formatter -> Block.t -> unit) ->
    ?ext_inline:(break_on_soft:bool -> Inline.t -> Inline.t) ->
    unit -> Format.formatter -> Block.t -> unit
  (** [pp_block_with ?ext ?ext_inline () ppf b] prints [b]. [ext] handles
      block extension constructors; [ext_inline] is forwarded to
      {!Inline.to_plain_text}. *)

  val pp_block : Format.formatter -> Block.t -> unit
  (** [pp_block] is {!pp_block_with} with defaults. *)

  val pp_inline_preview :
    ?ext_inline:(break_on_soft:bool -> Inline.t -> Inline.t) ->
    Format.formatter -> Inline.t -> unit
  (** Truncated, plain-text preview of an inline. *)
end

(** {1:struct Struct: colon-keyed tree restructuring} *)

(** Colon-keyed tree restructuring.

    A post-parse {!Doc.t} pass that turns list items and paragraphs carrying a
    colon-delimited label/value relationship into {!Block.extension-Ext_keyed}
    nodes.

    There are two forms. A {b trailing-colon} node ends with an unescaped [:]
    (no space after) and absorbs its body from indented sub-blocks or
    following content. An {b inline-value} node contains a [": "] split: the
    last segment is a free-form value, the preceding ones are labels. Each
    label must be a single inline unit (text, emphasis, strong emphasis, code
    span or autolink); the value is unrestricted.

    The pass is opt-in: nothing produces keyed nodes unless {!rewrite_doc} is
    called. *)
module Struct : sig
  val rewrite_doc : ?paragraph_inline_value:bool -> Doc.t -> Doc.t
  (** [rewrite_doc ?paragraph_inline_value doc] restructures [doc].

      [paragraph_inline_value] (default [true]) controls whether the
      inline-value form rewrites standalone paragraphs (outside list items)
      into {!Block.extension-Ext_keyed} nodes. When [false], only
      trailing-colon paragraphs and list-item keying apply. *)

  val unkey : Block.t -> Block.t
  (** [unkey b] flattens every {!Block.extension-Ext_keyed} node in [b] back to
      the plain blocks it stands for. Because a label keeps its [":"] separator
      as content, re-joining it to the value reproduces the pre-rewrite source
      modulo {!Inline.normalize}; hence [unkey] inverts {!rewrite_doc} up to
      normalisation, which is the precise sense in which the Struct pass is
      content-invisible. Renderers without keyed-node support can [unkey] a node
      and render the result as ordinary CommonMark. *)

  val label_key : Inline.t -> Inline.t
  (** [label_key label] is the bare key of a keyed node's [label], i.e. [label]
      with its trailing [":"] separator removed. A {!Inline.Text} key keeps the
      separator inline, so the trailing blanks and the single separator colon are
      dropped; a non-[Text] key arrives as [Inlines \[unit; Text sep\]] whose key
      is [unit]. This complements {!unkey} (which keeps the separator for
      content-invisibility): it is the accessor a {e semantic} renderer -- e.g.
      one emitting [<dl>]/[<dt>]/[<dd>] -- uses to recover the displayed key. *)
end

(** {1:extensions Extensions}

    For some documents, bare CommonMark just misses it. The extensions
    are here to make it hit the mark. To enable them use
    {!Doc.of_string} with [strict:false].

    Please note the following:
    {ol
    {- There is no plan to provide an extension mechanism at the
       parsing level. A lot can already be achieved by using
       {{!Label.resolvers}reference resolvers},
       abusing code fences, post-processing the abstract syntax tree, or
       {{!Cmarkit_renderer.example}extending} the renderers.}
    {- In order to minimize dialects and extension interaction
       oddities, there is no plan to allow to selectively
       enable extensions.}
    {- If one day the CommonMark specification standardizes a set
       of extensions. [Cmarkit] will support those.}
    {- In the short term, there is no plan to support more extensions than
       those that are listed here.}}

    {2:ext_wikilink Wikilinks}

    Obsidian-style wikilinks, enabled by the [wikilink] argument of
    {!Doc.of_string} (independently of [strict]).

    {v See [[Note#Heading|display]] and embed ![[Other Note]]. v}

    Text delimited by [ [[ ]] ] on a single line gets into an
    {!Inline.extension-Ext_wikilink} node; a leading [!] marks an embed. The
    content between the brackets is opaque (no nested inline parsing) and is
    split into an optional target, a [#]-fragment (a heading path, or a [^]
    block reference) and an optional [|] display text. See
    {!Inline.Wikilink}.

    {2:ext_strikethrough Strikethrough}

    According to {{:https://pandoc.org/MANUAL.html#strikeout}[pandoc]}.

    {v Strikethrough your ~~perfect~~ imperfect thoughts. v}

    Inline text delimited between two [~~] gets into an
    {!Inline.extension-Ext_strikethrough} node.

    The text delimited by [~~] cannot start or end with
    {{:https://spec.commonmark.org/0.31.2/#unicode-whitespace-character}
    Unicode whitespace}. When a closer can close multiple openers, the
    neareast opener is closed. Strikethrough inlines can be nested.

    {2:ext_math Math}

    According to a mix of
    {{:https://pandoc.org/MANUAL.html#extension-tex_math_dollars}
    [pandoc]}, {{:https://docs.gitlab.com/ee/user/markdown.html#math}GLFM},
    {{:https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/writing-mathematical-expressions}GFM}.

    {3:ext_math_inline Inline math}

    {v This is an inline $\sqrt(x - 1)$ math expression. v}

    Inline text delimited between [$] gets into an
    {!Inline.extension-Ext_math_span} node.

    The text delimited by [$] cannot start and end with
    {{:https://spec.commonmark.org/0.31.2/#unicode-whitespace-character}
    Unicode whitespace}. Inline math cannot be nested, after an opener
    the nearest (non-escaped) closing delimiter matches. Otherwise it
    is parsed in essence like a
    {{:https://spec.commonmark.org/0.31.2/#code-spans}code span}.

    {3:ext_math_display Display math}

{v
It's better to get that $$ \left( \sum_{k=1}^n a_k b_k \right)^2 $$
on its own line. A math block may also be more convenient:

```math
\left( \sum_{k=1}^n a_k b_k \right)^2 < \Phi
```
v}

    Inline text delimited by [$$] gets into a
    {!Inline.extension-Ext_math_span} with the
    {!Inline.Math_span.display} property set to [true]. Alternatively
    code blocks whose
    {{!Block.Code_block.language_of_info_string}language} is [math]
    get into in {!Block.Ext_math_block} blocks.

    In contrast to [$], the text delimited by [$$] can start and end
    with whitespace, however it can't contain a blank line. Display
    math cannot be nested, after an opener the nearest (non-escaped)
    closing delimiter matches. Otherwise it's parsed in essence like
    a {{:https://spec.commonmark.org/0.31.2/#code-spans}code span}.

    {2:ext_list_task_items List task items}

    According to a mix of
    {{:https://github.com/mity/md4c/blob/master/test/spec-tasklists.txt}md4c},
    {{:https://docs.gitlab.com/ee/user/markdown.html#task-lists}GLFM},
    {{:https://github.github.com/gfm/#task-list-item}GFM} and personal
    ad-hoc brewery.

{v
* [ ] That's unchecked.
* [x] That's checked.
* [~] That's cancelled.
v}

    If a list item starts with up to three space, followed by followed
    by [\[], a single Unicode character, [\]] and a space (the space
    can be omitted if the line is empty, but subsequent indentation
    considers there was one). The Unicode character gets stored in
    {!Block.List_item.ext_task_marker} and counts as one column
    regardless of the character's render width. The task marker
    including the final space is not considered part of the list marker as
    far as subsequent indentation is concerned.

    The Unicode character indicates the status of the task. That's up
    to the client but the function
    {!Block.List_item.task_status_of_task_marker} which is used by the
    built-in renderers makes the following choices:

    {ul
    {- Unchecked: [' '] (U+0020).}
    {- Checked: ['x'] (U+0078), ['X'] (U+0058), ['✓'] (U+2713, CHECK MARK),
       ['✔'] (U+2714, HEAVY CHECK MARK), ['𐄂'] (U+10102, AEGEAN CHECK MARK),
       ['🗸'] (U+1F5F8, LIGHT CHECK MARK).}
    {- Cancelled: ['~'] (U+007E).}
    {- Other: any other character, interpretation left to clients or
       renderers (built-in ones equate it with done).}}

    {2:ext_tables Tables}

    According to {{:https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#pipe-table}djot}.

{v
| # |      Name | Description           |                    Link |
|:-:|----------:|:----------------------|------------------------:|
| 1 |     OCaml | The OCaml website     |     <https://ocaml.org> |
| 2 |   Haskell | The Haskell website   |   <https://haskell.org> |
| 3 |       MDN | Web dev docs | <https://developer.mozilla.org/> |
| 4 | Wikipedia | The Free Encyclopedia | <https://wikipedia.org> |
v}

    A table is a sequence of rows, each row starts and ends with a
    (non-escaped) pipe [|] character. The first row can't be indented
    by more than three spaces of indentation, subsequent rows can be
    arbitrarily indented. Blanks after the final pipe are allowed.

    Each row of the table contains cells separated by (non-escaped)
    pipe [|] characters. Pipes embedded in inlines constructs do not
    count as separators (the parsing strategy is to parse the row as
    an inline, split the result on the [|] present in {e toplevel}
    text nodes and strip initial and trailing blanks in cells). The
    number of [|] separators plus 1 determines the number of columns
    of a row. The number of columns of a table is the greatest number
    of columns of its rows.

    A separator line is a row in which every cell content is made only
    of one or more [-] optionally prefixed and suffixed by [:]. These
    rows are not data, they indicate alignment of data in their cell
    for subsequent rows (multiple separator lines in a single table
    are allowed) and that the previous line (if any) was a row of
    column headers. [:-] is left aligned [-:] is right aligned, [:-:]
    is centered. If there's no alignement specified it's left
    aligned.

    Tables are stored in {!Block.extension-Ext_table} nodes.

    {2:ext_footnotes Footnotes}

    According to {{:https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#footnotes}djot} for the footnote contents.

{v
This is a footnote in history[^1] with mutiple references[^1].
Footnotes are not [very special][^1] references.

 [^1]: Footnotes can have
lazy continuation lines and multiple paragraphs.

  If you start one column after the left bracket, blocks still get
  into the footnote.

 But this is no longer the footnote.
v}

    Footnotes go through the label resolution mecanism and share the
    same namespace as link references (including the [^]). They end up
    being defined in the {!Doc.defs} as {!Block.Footnote.Def}
    definitions. Footnote references are simply made by using
    {!Inline.extension-Link} with the corresponding labels.

    {3:ext_footnote_def Definition}

    A footnote definition starts with a (single line)
    {{:https://spec.commonmark.org/0.31.2/#link-label}link label}
    followed by [:]. The label must start with a [^]. Footnote labels
    go through the label {{!Label.resolvers}resolution} mechanism.

    All subsequent lines indented one column further than the start of
    the label (i.e. starting on the [^]) get into the footnote. Lazy
    continuation lines are supported.

    The result is stored in the document's {!Doc.defs} in
    {!Block.Footnote.Def} cases and it's position in the documentation
    witnessed by a {!Block.extension-Ext_footnote_definition} node which
    is kept for layout.

    {3:ext_footnote_ref References}

    Footnote references are simply reference links with the footnote
    label. Linking text on footnotes is allowed. Shortcut and
    collapsed references to footnotes are rendered specially by
    {!Cmarkit_html}. *)
