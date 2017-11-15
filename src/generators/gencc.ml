let guard type_path =
	String.uppercase_ascii
	(match type_path with
		| [], type_name -> type_name
		| package, type_name -> String.concat "_" package ^ "_" ^ type_name
	)

let generate ctx =
	(* List.iter (fun mtype -> generate_module_type mtype ctx.file) ctx.types; *)
	print_endline("Generating cc");
	()