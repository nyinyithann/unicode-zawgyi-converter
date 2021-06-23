open Alcotest
open Unicode_zawgyi_converter

let read_file filename =
  try
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  with End_of_file -> ""

let write s =
  let oc = open_out "converted_zg.txt" in
  Printf.fprintf oc "%s" s;
  close_out oc

let test_uni2zg () =
  let input = "\u{1004}\u{103a}\u{1039} abc \u{1004}\u{103a}\u{1039} def" in
  let zawgyi_text = "\u{1064} abc \u{1064} def" in
  let converted_text = Converter.uni2zg input in
  check string "same string" converted_text zawgyi_text

let test_zg2uni () =
  let input = "\u{1088}" in
  let unicode_text = "\u{103e}\u{102f}" in
  let converted_text = Converter.zg2uni input in
  check string "same string" converted_text unicode_text

let suite =
  [
    ("unicode to zawgyi", `Quick, test_uni2zg);
    ("zawgyi to unicode", `Quick, test_zg2uni);
  ]
