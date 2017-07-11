open Str
open Ic
open Lex_prop
open Grammar_prop

(* Parse the propositional formula by launching the lexer and the parser of propositional logic *)
let parse_prop formula = 
	let lexbuf = Lexing.from_string formula in
	Grammar_prop.terminated_expr Lex_prop.token lexbuf

(* Take a string and returns the string without all white-space characters *)
let strip str = 
  let str = Str.global_replace (Str.regexp " +") "" str in
  Str.replace_first (Str.regexp " +$") "" str

(* Read the input file and return the parameters read *)
let read_input file_name = 
	(* Regexpr for list of list of int, name of the parameters, list of int and int *)
	let int_list_list_regexp = Str.regexp "\\(['('][0-9][0-9]*\\([',' ';'][0-9][0-9]*\\)*[')']\\)+" in
	let variable_regexp = Str.regexp "initial_beliefs\\|graphe\\|question_order\\|ic" in
	let int_list_regexp = Str.regexp "['('][0-9][0-9]*\\([',' ';'][0-9][0-9]*\\)*[')']" in
	let int_regexp = Str.regexp "[0-9][0-9]*" in

	(* Global variable that will be updating during the parsing *)
	let initial_b = ref [] in
	let graph = ref [] in
	let quest_order = ref [||] in 
	let ic_from_file = ref TOP in

	(* Parse int in a string and returns them in a list *)
	let rec parse_int s i res = 
		try
			ignore(search_forward int_regexp s i);
			parse_int s (match_end ()) (res @ [(int_of_string (matched_string s))])
		with
			Not_found -> [res]
	in

	(* Parse a list of number and returns the list of list *)
	let rec parse_list s i res =
		try
			ignore(search_forward int_list_regexp s i);
			let cur_i = match_end () in
			parse_list s (cur_i) (res @ parse_int (matched_string s) 0 []) 
		with 
			Not_found -> res
	in

	(* Transform an int list list to a list of couple *)
	let int_list_list_to_couple_list l =
		let aux_int l =
			if (List.length l = 2) then
				(List.hd l, List.hd (List.tl l))
			else
				failwith ("Parsing error: some couple in the graphe are not couples")
		in
		let rec aux_list l res =
			match l with
			| [] -> res
			| h :: t -> aux_list t (res @ [aux_int h])
		in
		aux_list l []
	in

	(* Transform an int list list into an int array array *)
	let int_list_list_to_array_array l =
		let rec aux_list l res =
			match l with
			| [] -> res
			| h :: t -> aux_list t (Array.append res ([|Array.of_list h|]))
		in
		aux_list l [||]
	in

	(* Create the initial beliefs according to the input file *)
	let create_initial s =
		parse_list s 0 []
	in

	(* Create the graph according to the input file *)
	let create_graphe s =
		int_list_list_to_couple_list (parse_list s 0 [])
	in

	(* Create the question order according to the input file *)
	let create_order s = 
		int_list_list_to_array_array (parse_list s 0 [])
	in

	(* Parse the input file *)
	let parse_input line raw_line = 
		match string_match variable_regexp line 0 with
		| false -> ()
		| true -> (
			let cur_var = matched_string line in
			match cur_var with
				| s when s = "ic" -> ic_from_file := parse_prop (String.sub raw_line 3 (String.length raw_line - 3))
				| _ -> ( 
					(try
						ignore(search_forward int_list_list_regexp line 0)
					with 
						Not_found -> failwith("Parsing error: input file corrupted !"));
					(match cur_var with
					| s when s = "initial_beliefs" -> initial_b := create_initial (matched_string line)
					| s when s = "graphe" -> graph := create_graphe (matched_string line)
					| _ -> quest_order := create_order (matched_string line)
				)
			)
		)
	in

	(* Read the entire input file *)
	let rec read_file file =
		try
			let cur_line = input_line file in
			parse_input (String.lowercase (strip cur_line)) cur_line;
			read_file file
		with
			End_of_file -> close_in file
	in

	let file = open_in file_name in
	read_file file;
	(!initial_b, !graph, !quest_order, !ic_from_file)