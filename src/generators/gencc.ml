open Type

type code_element =
				| Statement of string
				| StatementBlock of string * code_block
				| Line of string
				| NewLine
				| StatementIf of
					code_element * code_element option
				| CodeBlock of code_block
and code_block = code_element list

let rec s_ce ce =
	let rec s_cb cb = String.concat "" (List.map (fun x -> s_ce x) cb) in
	match ce with
	| Line s -> s ^ "\n"
	| Statement s -> s ^ ";\n"
	| StatementBlock (s, cb) -> s ^ " {\n" ^ s_cb cb ^ "}\n"
	| CodeBlock cb -> s_cb cb
	| _ -> "unknown code"

let guard type_path =
	String.uppercase_ascii
	(match type_path with
		| [], type_name -> type_name
		| package, type_name -> String.concat "_" package ^ "_" ^ type_name
	)

let guard_begin gid =
	CodeBlock [Line ("#ifndef " ^ gid); Line ("#define " ^ gid)]
let guard_end gid = Line ("#endif  // " ^ gid)
let inc_deps aoeu = NewLine
let s_cl_name a = ""
let s_cl_extends a = ""
let cl_field = NewLine
let cl_ctor opt = NewLine
let cl_decl cl = CodeBlock [
	StatementBlock (
	"class " ^ (s_cl_name cl.cl_path) ^ (s_cl_extends cl), [
		cl_ctor cl.cl_constructor;
		cl_field;
	]);
	Statement "";
]

let cl_header cl =
	let gid = guard cl.cl_path in
	CodeBlock [
		guard_begin gid;
		inc_deps "";
		cl_decl cl;
		guard_end gid;
	]

let cl_fwd (p, t) = List.fold_right (fun x ce -> StatementBlock ("namespace " ^ x, [ce])) p (Statement ("class " ^ t))

let generate ctx =
	(* List.iter (fun mtype -> generate_module_type mtype ctx.file) ctx.types; *)
	print_endline("Generating cc");
	()
