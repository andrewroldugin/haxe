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

let test_guard_empty_package () =
	let actual = guard ([], "Type") in
	streq "TYPE" actual

let test_guard () =
	let actual = guard (["path"; "to"], "Type") in
	streq "PATH_TO_TYPE" actual

#stub guard_begin Line "<guard_begin>"
#stub guard_end Line "<guard_end>"
#stub inc_deps Line "[<include_deps>]"
#stub cl_decl Line "<class_decl>"
#setup s_cl
let test_class_header () =
	let expected = fixture "class_header.dat" in
	let actual = s_cl "" in
	streq expected actual
#teardown