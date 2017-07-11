open Players
open Custom_print
open Pod
open Ic
open Arguments

(* The pwod process update *)
let rec pwod_update f ic initial_state graphe nb_questions nb_players question_order i =
	(* Update all questions for a player *)
	let update_player f ic player state graphe question =
		match f graphe player state question with
		| Some a -> let new_opinion = set_question_answer (get_player_answers state player) question a in
			(*Printf.printf "NEW OPINION";
			print_int_list new_opinion;
			Printf.printf "\n";*)
			if appartient_list new_opinion ic then
				new_opinion
			else
				get_player_answers state player
		| _ -> get_player_answers state player 
	in

	(* Update all players *)
	let rec pwod_update_aux f ic state graphe nb_players nb_questions question_order player question res j =
		(*Printf.printf "========================= p=%d q=%d\n" player question;
		print_state res;*)
		match player with
		| a when a = nb_players -> if question = nb_questions - 1 then
				(res, j)
			else
				((if !verbose && (not !in_manipulation) then
					(Printf.printf "\n\n ======= PWOD STATE %i ======= \n" j;
					print_state res)
				else 
					());
				pwod_update_aux f ic res graphe nb_players nb_questions question_order 0 (question + 1) [] (j + 1))
		| a -> pwod_update_aux f ic state graphe nb_players nb_questions question_order (player + 1) (question) (res @ [update_player f ic a state graphe question_order.(player).(question)]) j
	in
	let cur_update = pwod_update_aux f ic initial_state graphe nb_players nb_questions question_order 0 0 [] i in
	match (fst cur_update) with
	| state when state = initial_state -> state
	| state -> 
		pwod_update f ic state graphe nb_questions nb_players question_order (snd cur_update)