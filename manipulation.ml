open Pod
open Pwod
open Players
open Custom_print

(* Returns the Hamming distance between two states *)
let hamming_distance state1 state2 =
	let rec hamming_distance_aux state1 state2 res =
		match (state1, state2) with
		| ([], []) -> res
		| (h1 :: t1, h2 :: t2) -> hamming_distance_aux t1 t2 (res + abs(h1 - h2))
		| _ -> failwith("The two states do not have the same length !")
	in 
	hamming_distance_aux state1 state2 0 

(* Find the best strategy in the POD process, if truth brings the same Hamming Distance, then returns true*)
let best_pod_strategy f initial ic graphe nb_question nb_players true_beliefs =
	let rec best_pod_strategy_aux f state ic graphe nb_question nb_players true_beliefs res_h res_strat possibilities =
		match possibilities with
		| [] -> (res_h, res_strat)
		| h :: t -> (*Printf.printf "\n\n =============== Player 1 plays ";
			print_int_list h;
			Printf.printf "===============\n\n";*)
			let new_state = set_player_answers state 1 h in
			let pod = pod_update f ic new_state graphe nb_question nb_players 1 in
			let ham = hamming_distance (get_player_answers pod 0) (get_player_answers true_beliefs 1) in
			(*Printf.printf "\n\n =============== Which is a distance %d ===============\n\n" ham ;*)
			if ham <= res_h then
				best_pod_strategy_aux f state ic graphe nb_question nb_players true_beliefs ham h t
			else
				best_pod_strategy_aux f state ic graphe nb_question nb_players true_beliefs res_h res_strat t
	in
	best_pod_strategy_aux f initial ic graphe nb_question nb_players true_beliefs (nb_question + 1) [] (ic @ [get_player_answers true_beliefs 1])

(* Returns the best strategy for the player 1 in the PWOD process *)
let best_pwod_strategy f initial ic graphe nb_question nb_players true_beliefs question_order =
	let rec best_pwod_strategy_aux f state ic graphe nb_question nb_players true_beliefs question_order res_h res_strat possibilities =
		match possibilities with
		| [] -> (res_h, res_strat)
		| h :: t -> (*Printf.printf "\n\n =============== Player 1 plays ";
			print_int_list h;
			Printf.printf "===============\n\n";*)
			let new_state = set_player_answers state 1 h in
			let pwod = pwod_update f ic new_state graphe nb_question nb_players question_order 1 in
			let ham = hamming_distance (get_player_answers pwod 0) (get_player_answers true_beliefs 1) in
			(*Printf.printf "\n\n =============== Which is a distance %d ===============\n\n" ham ;*)
			if ham <= res_h then
				best_pwod_strategy_aux f state ic graphe nb_question nb_players true_beliefs question_order ham h t
			else
				best_pwod_strategy_aux f state ic graphe nb_question nb_players true_beliefs question_order res_h res_strat t
	in
	best_pwod_strategy_aux f initial ic graphe nb_question nb_players true_beliefs question_order (nb_question + 1) [] (ic @ [get_player_answers true_beliefs 1])