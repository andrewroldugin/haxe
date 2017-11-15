open OUnit

#use "../../src/generators/gencc.ml"

let streq s1 s2 =
	assert_equal s1 s2 ~printer:(fun s -> "\n\n" ^ s ^ "\n\n")

let read_file f =
	let ic = open_in f in
	let n = in_channel_length ic in
	let s = Bytes.create n in
	really_input ic s 0 n;
	close_in ic;
	s

let fixture_dir = "data/"

let fixture file = read_file (fixture_dir ^ file)

let test_fixture = "Gen" >::: [
]

(* Tests *)

let _ = run_test_tt ~verbose:false test_fixture
