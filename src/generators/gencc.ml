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

let rec s_ce = function
	| Line s -> s ^ "\n"
	| CodeBlock cb -> String.concat "" (List.map (fun x -> s_ce x) cb)
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
let cl_decl aeou = NewLine

let s_cl cl =
	let gid = guard cl.cl_path in
	(s_ce (guard_begin gid)) ^
	(s_ce (inc_deps "")) ^
	(s_ce (cl_decl "")) ^
	(s_ce (guard_end gid))

let generate ctx =
	(* List.iter (fun mtype -> generate_module_type mtype ctx.file) ctx.types; *)
	print_endline("Generating cc");
	()