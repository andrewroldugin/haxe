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

#stub guard_begin L "<guard_begin>"
#stub guard_end L "<guard_end>"
#stub inc_deps L "[<include_deps>]"
#stub cl_decl L "<class_decl>"
#setup cl_header
let test_class_header () =
	let expected = fixture "class_header.dat" in
	let actual = s_ce (cl_header Type.null_class) in
	streq expected actual
#teardown

let test_guard_begin () =
	let expected = fixture "guard_begin.dat" in
	let actual = s_ce(guard_begin "GUARDID") in
	streq expected actual

let test_guard_end () =
	let expected = fixture "guard_end.dat" in
	let actual = s_ce(guard_end "GUARDID") in
	streq expected actual

#stub s_cl_name "<class_name>"
#stub s_cl_extends "[<base_classes>]"
#stub cl_ctor L "[<ctor>]"
#stub cl_field L (if static then "[<static>]" else "[<field>]")
#setup cl_fields
#setup cl_decl
let test_class_decl () =
	let expected = fixture "class_decl.dat" in
	(*let c = Type.mk_class Type.null_module ([],"") Globals.null_pos in*)
	let c = Type.null_class in
	c.cl_ordered_fields <- [Type.null_field];
	c.cl_ordered_statics <- [Type.null_field];
	let actual = s_ce (cl_decl c) in
	streq expected actual
#teardown
#setup cl_fields

let test_cl_fwd () =
	let expected = fixture "class_fwd.dat" in
	let actual = s_ce (cl_fwd (["path"; "to"], "Type")) in
	streq expected actual

let test_cl_name () =
	streq "Type" (s_cl_name ([], "Type"));
	streq "path::to::Type" (s_cl_name (["path"; "to"], "Type"))
