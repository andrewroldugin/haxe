open Type
open ExtList

type code_element =
				| S of string                (* Statement *)
				| SB of string * code_block  (* StatementBlock *)
				| L of string                (* Line *)
				| NL                         (* NewLine *)
				| If of                      (* Statement If *)
					code_element * code_element option
				| CB of code_block           (* Code Block *)
and code_block = code_element list

let rec s_ce ?(i = "") ce =
	let rec s_cb cb i = String.concat "" (List.map (fun x -> s_ce x ~i:i) cb) in
	match ce with
	| L s -> i ^ s ^ "\n"
	| S s -> i ^ s ^ ";\n"
	| SB (s, cb) -> i ^ s ^ " {\n" ^ s_cb cb (i ^ "\t") ^ i ^ "}\n"
	| CB cb -> s_cb cb i
	| _ -> "unknown code"

let s_path (p, t) delim = String.concat delim (p@[t])
let guard type_path = String.uppercase_ascii (s_path type_path "_")

let guard_begin gid =
	CB [L ("#ifndef " ^ gid); L ("#define " ^ gid)]
let guard_end gid = L ("#endif  // " ^ gid)
let inc_deps deps = CB (List.map (fun x -> L ("#include \"" ^ x ^ "\"")) deps)
let s_cl_name path = s_path path "::"
let t_path = function
	| TAbstract({a_path = p},_) -> p
	| _ -> ([], "")
let s_t_name t = s_path (t_path t) "::"
let is_ptr_t = function
	| TAbstract({a_path = [], "Dynamic"},_) -> true
	| _ -> false
let s_ptr_t t = if (is_ptr_t t) then "*" else ""
let s_t t = s_t_name t ^ s_ptr_t t
let s_access public = if public then "public:" else "protected:"
let s_static s = if s then "static " else ""
let s_params params = "()"
let s_cl_extends a = ""
let cl_field f s =
	S (s_access f.cf_public ^ " " ^ s_static s ^ s_t f.cf_type ^ " " ^ f.cf_name ^
		s_params f.cf_params)
let cl_ctor opt = NL
let cl_fields fs static = CB (List.map (fun f -> cl_field f static) fs)
let cl_decl cl = CB [
	SB (
	"class " ^ (s_cl_name cl.cl_path) ^ (s_cl_extends cl), [
		cl_ctor cl.cl_constructor;
		cl_fields cl.cl_ordered_fields false;
		cl_fields cl.cl_ordered_statics true;
	]);
	S "";
]
let path_file p = s_path p "/"
let t_file t = path_file (t_path t)
let add_ext files ext = List.map (fun f -> f ^ ext) files
let cl_decl_deps cl =
	let direct_deps =
		let fields_deps = List.filter_map
		(fun f -> match f.cf_kind with
			| Var _ when (not (is_ptr_t f.cf_type)) ->
				Some (t_file f.cf_type)
			| _ -> None
		)
			cl.cl_ordered_fields
		in
		let extends_deps = [] in
		List.unique (extends_deps @ fields_deps)
	in
	let fwd_deps =
		let fields_deps = List.flatten
			(List.map
				(fun f ->
					(t_file f.cf_type)::(List.map (fun (_, pt) -> t_file pt) f.cf_params)
				)
				(cl.cl_ordered_fields @ cl.cl_ordered_statics)
			)
		in
		let cl_fwd = path_file cl.cl_path in
		let deps = List.unique (cl_fwd :: fields_deps) in
		List.filter (fun x -> not (List.mem x direct_deps)) deps
	in
	List.sort (add_ext fwd_deps ".fwd.h" @ add_ext direct_deps ".h")

let cl_header cl =
	let gid = guard cl.cl_path in
	CB [
		guard_begin gid;
		inc_deps (cl_decl_deps cl);
		cl_decl cl;
		guard_end gid;
	]

let cl_fwd (p, t) =
	List.fold_right (fun x ce -> SB ("namespace " ^ x, [ce])) p (S ("class " ^ t))

let generate ctx =
	(* List.iter (fun mtype -> generate_module_type mtype ctx.file) ctx.types; *)
	print_endline("Generating cc");
	()
