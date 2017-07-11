(* Give a list of influencers for a given player in a given graph *)
let influencers graphe player =
	let rec influencers_aux graphe player res = 
		match graphe with
		| [] -> res
		| h :: t when snd h = player -> influencers_aux t player (res @ [(fst h)])
		| h :: t -> influencers_aux t player res
	in
	influencers_aux graphe player []

(* Returns all answers from a given player in a given state *)
let get_player_answers state player =
	let rec get_player_answers_aux state player index = 
		match state with
		| [] -> failwith ("The player " ^ string_of_int player ^ " has not been found in this state !")
		| h :: t -> if index = player then h else get_player_answers_aux t player (index + 1)
	in
	get_player_answers_aux state player 0

(* Returns the answer to the given question from the given player in the given state *)
let get_answer state player question =
	let rec get_answer_aux answers question index =
		match answers with
		| [] -> failwith ("The answer of the question " ^ string_of_int question ^ " has not been found !")
		| h :: t -> if index = question then h else get_answer_aux t question (index + 1)
	in
	get_answer_aux (get_player_answers state player) question 0

(* print_state a given player belief with an aswer to all questions in a given state *)
let set_player_answers state player value =
	let rec set_player_answers_aux state player value res index =
		match state with
		| [] -> res
		| h :: t when index = player -> set_player_answers_aux t player value (res @ [value]) (index + 1)
		| h :: t -> set_player_answers_aux t player value (res @ [h]) (index + 1)
	in
	set_player_answers_aux state player value [] 0

(* print_state only one question in a given profile *)
let set_question_answer answers question value =
	let rec set_question_answer_aux answers question value res index =
		match answers with
		| [] -> res
		| h :: t when index = question -> set_question_answer_aux t question value (res @ [value]) (index + 1)
		| h :: t -> set_question_answer_aux t question value (res @ [h]) (index + 1)
	in
	set_question_answer_aux answers question value [] 0