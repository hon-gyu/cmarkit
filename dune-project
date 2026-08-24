(lang dune 3.22)

(name oymarkit)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name oymarkit)
 (allow_empty)
 (synopsis "Oymarkit: extended cmarkit")
 (description
  "")
 (depends
  ocaml
  (dune
   (>= 3.22))
  cmarkit
  core
  yaml
  yojson
  ppx_jane
  ppx_string
  ppx_quick_test
  ppx_yojson_conv
  (ppx_expect :with-test)))
