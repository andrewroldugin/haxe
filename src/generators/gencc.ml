type code_element =
				| Statement of string
				| StatementBlock of string * code_block
				| Line of string
				| NewLine
				| StatementIf of
					code_element * code_element option
				| CodeBlock of code_block
and code_block = code_element list

let s_ce = function
	| Line s -> s ^ "\n"
	| _ -> "unknown code"

let guard type_path =
	String.uppercase_ascii
	(match type_path with
		| [], type_name -> type_name
		| package, type_name -> String.concat "_" package ^ "_" ^ type_name
	)

let guard_begin type_path = NewLine
let guard_end type_path = NewLine
let inc_deps aoeu = NewLine
let cl_decl aeou = NewLine

let s_cl cl =
	(s_ce (guard_begin "")) ^
	(s_ce (inc_deps "")) ^
	(s_ce (cl_decl "")) ^
	(s_ce (guard_end ""))

let generate ctx =
	(* List.iter (fun mtype -> generate_module_type mtype ctx.file) ctx.types; *)
	print_endline("Generating cc");
	()