open Pod
open Pwod
open Ic
open Players
open Aggregation_functions
open Custom_print
open Printf
open Manipulation
open Arg
open Arguments
open Parser

(* Check if the given state satisfy the integrity constraint *)
let rec check_state_integrity state ic =
	match state with
	| [] -> true
	| h :: t -> if appartient_list h ic then 
			check_state_integrity t ic 
		else 
			failwith("This state does not satisfy the integrity constraint !")

(* Check if the influence graph given is coherent: every player in the graph is defined in the initial beliefs *)
let rec check_graphe_integrity graphe nb_players =
	match graphe with
	| [] -> true
	| h :: t -> if fst h < nb_players && snd h < nb_players then
			check_graphe_integrity t nb_players
		else
			failwith("There more players in the graph than in the initial beliefs !")

(* Check for the integrity of the question order given for the PWOD process: every player has an order
   and the order is complete for every player *)
let check_order_integrity question_order nb_players nb_question =
	(* Check is the order is complete *)
	let rec check_every_question player_order player =
		let bool_array = Array.make (Array.length player_order) 1 in
		(for i = 0 to (Array.length player_order - 1) do
			if bool_array.(player_order.(i)) = 1 then
				bool_array.(player_order.(i)) <- 0
			else 
				failwith("Error in question order for player " ^ string_of_int player ^ " in the question " ^ string_of_int i ^ " !")
		done)
	in
	(* Check if every player has an order *)
	let rec check_every_players questions nb_players i = 
		(for i = 0 to (nb_players - 1) do
			if Array.length questions.(i) = nb_question then
				check_every_question questions.(i) i
			else
				failwith("Player " ^ string_of_int i ^ " has not an order for every question !")
		done);
		true
	in
	if Array.length question_order < nb_players then
		failwith("Not all players have a question order !")
	else (
		if Array.length question_order > nb_players then
			failwith("There are too many players in the question order !")
		else 
			check_every_players question_order nb_players 0
	)

let _ =
	let spec_list = 
		[("-v", Unit (fun () -> verbose := true), "Verbose mode to display all steps in processes");
		("-pod", Unit (fun () -> do_pwod := false), "Display only results for the POD process");
		("-pwod", Unit (fun () -> do_pod := false), "Display only results for the PWOD process");
		("-m", Unit (fun () -> do_mani := false), "Disable the computation of the manipulation");
		("-i", Arg.String (fun a -> input_file := a), "Select the file from which input will be read, defaut file is \"input\"")] in
	parse spec_list print_endline "Opinion diffusion processes";

	let (input_init, input_graphe, input_order, input_ic) = read_input !input_file in

	let initial = input_init in
	let nb_question = List.length (List.hd initial) in
	let nb_players = List.length initial in
	let prop_ic = input_ic in
	let ic = create_ic prop_ic nb_question in
	let g = input_graphe in
	let question_order = input_order in
	
	printf "\nChecking if initial state satisfies IC: ";
	(if check_state_integrity initial ic then
		printf "OK\n"
	else
		());
	printf "Checking if the graphe is consistant with the intial beliefs: ";
	(if check_graphe_integrity g nb_players then
		printf "OK\n"
	else
		());
	printf "Checking the integrity of the question order: ";
	(if check_order_integrity question_order nb_players nb_question then
		printf "OK\n"
	else
		());

	print_initial g initial prop_ic;

	(if !do_pod then
		((if !verbose then
			(printf "\n\n ======= POD STATE 0 =======\n";
			print_state initial)
		else 
			());
		let pod = pod_update (fun i j k l -> strict_maj i j k l) ic initial g nb_question nb_players 1 in
		let best_pod = ref (0, []) in
		(if !do_mani then
			(in_manipulation := true;
			best_pod := best_pod_strategy (fun i j k l -> strict_maj i j k l) initial ic g nb_question nb_players initial;
			in_manipulation := false)
		else 
			());
		print_best_pod pod (!best_pod) initial (hamming_distance (get_player_answers pod 0) (get_player_answers initial 1)))
	else 
		());

	(if !do_pwod then
		((if !verbose then
			(printf "\n\n ======= PWOD STATE 0 =======\n";
			print_state initial)
		else 
			());
		let pwod = pwod_update (fun i j k l -> strict_maj i j k l) ic initial g nb_question nb_players question_order 1 in
		let best_pwod = ref (0, []) in
		(if !do_mani then
			(in_manipulation := true;
			best_pwod := best_pwod_strategy (fun i j k l -> strict_maj i j k l) initial ic g nb_question nb_players initial question_order; 
			in_manipulation := false)
		else 
			());
		print_pwod pwod (!best_pwod) initial (hamming_distance (get_player_answers pwod 0) (get_player_answers initial 1)))
	else
		())