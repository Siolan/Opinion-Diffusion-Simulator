open Players 
open Custom_print
open Ic
open Arguments

(* Take an int and create a list from 0 to this int *)
let create_int_list n =
	let rec create_int_list_aux n i res =
		if n = i then
			res
		else
			create_int_list_aux n (i + 1) (res @ [i])
	in
	create_int_list_aux n 0 []

(* The update method for the pod update *)
let rec pod_update f ic initial_state graphe nb_question nb_players i =
	(* Update all questions for a player *)
	let rec update_player f ic player graphe state question_id res =
		match question_id with
		| [] -> (*Printf.printf "Player %d, on a " player;
			print_int_list res;
			Printf.printf "\n";*)
			if appartient_list res ic then
				res
			else 
				get_player_answers state player
		| h :: t -> (match f graphe player state h with
			(* We make the assumption that when there is a tie, we keep the previous value *)
			| None -> (*Printf.printf "Player %d: None" player;
				print_int_list (get_player_answers state player);
				Printf.printf "\n";*)
				get_player_answers state player
			| Some a -> update_player f ic player graphe state t (res @ [a])
		)
	in 
	(* Upadte all players *)
	let rec pod_update_aux f ic graphe state player_id nb_question res =
		match player_id with
		| [] -> (*Printf.printf "==============\n";
			print_state res;*)
			res
		| h :: t -> (*Printf.printf "=== Player %d ===\n" h;*)
			pod_update_aux f ic graphe state t nb_question (res @ [update_player f ic h graphe state (create_int_list nb_question) []])
	in
	match pod_update_aux f ic graphe initial_state (create_int_list nb_players) nb_question [] with
	| state when state = initial_state -> state
	| state -> 
		(if !verbose && (not !in_manipulation) then
			(Printf.printf "\n\n ======= POD STATE %i =======\n" i;
			print_state state)
		else 
			());
		pod_update f ic state graphe nb_question nb_players (i + 1)

