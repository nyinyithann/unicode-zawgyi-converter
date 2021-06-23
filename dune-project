(lang dune 2.0)
(name unicode-zawgyi-converter)
(documentation "https://nyinyithann.github.io/unicode-zawgyi-converter/")
(source (github nyinyithann/unicode-zawgyi-converter))
(license MIT)
(authors "nyi nyi thann")
(maintainers "nyi nyi thann")

(generate_opam_files true)

(package
 (name unicode-zawgyi-converter)
 (synopsis "Converter for Myanmar (Burmese) unicode text to zawgyi text, and vice versa")
 (description "\
Converter for Myanmar (Burmese) unicode text to zawgyi text, and vice versa
")
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 2.0))
  (alcotest :with-test)
  (odoc :with-doc)))
