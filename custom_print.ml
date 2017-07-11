open Printf
open Ic
open Players
open Arguments

(* Print a list of int *)
let print_int_list l =
	let rec print_list_aux l = 
		match l with
		| [] -> ()
		| h :: t when t = [] -> printf "%d" h;
			print_list_aux t
		| h :: t -> printf "%d, " h;
			print_list_aux t
	in
	printf "[";
	print_list_aux l; 
	printf "]"

(* Print all opinion of players for the given state *)
let print_state state =
	let rec print_state_aux state i =
		match state with
		| [] -> ()
		| h :: t -> printf "Player %d :\n" i;
			print_int_list h;
			printf "\n";
			print_state_aux t (i + 1)
	in
	print_state_aux state 0

(* Print the integrity constraint in logical form *)
let print_prop_ic ic = 
	let rec print_prop_ic_aux ic =
		match ic with
		| TOP -> printf "T"
		| BOT -> printf "⊥"
		| VAR a -> printf "%d" a
		| CONJ (a, b) -> print_bin_op "∧" a b
		| DISJ (a, b) -> print_bin_op "∨" a b
		| IMP (a, b) -> print_bin_op "⇒" a b
		| EQUIV (a, b) -> print_bin_op "⟺" a b
		| XOR (a, b) -> print_bin_op "⊕" a b
		| NOT a -> printf "¬";
			print_prop_ic_aux a
	and print_bin_op op a b =
		printf "(";
		print_prop_ic_aux a;
		printf " %s " op;
		print_prop_ic_aux b;
		printf ")";
	in
	print_prop_ic_aux ic

(* Print a graphe in a way that shows initial beliefs and relation between players *)
let rec print_graphe g initial =
	match g with
	| [] -> printf "\n"
	| h :: t -> printf "Player %d " (fst h);
		print_int_list (get_player_answers initial (fst h));
		printf " -----> Player %d " (snd h);
		print_int_list (get_player_answers initial (snd h));
		printf "\n";
		print_graphe t initial

(* Print an array *)
let print_tab tab =
	printf "\n[";
	(for i = 0 to (Array.length tab - 1) do
		printf "%s " (string_of_int tab.(i))
	done);
	printf "]\n"

(* Print the result of the POD process *)
let print_initial graphe initial ic =
	printf "\n ======= GRAPHE AND INITIAL BELIEFS =======\n\n";
	print_graphe graphe initial;
	printf " ======= IC =======\n\n";
	print_prop_ic ic

(* Print the result for the best POD solution *)
let print_best_pod pod_final best true_beliefs h_true =
	printf "\n\n ======= POD DIFFUSION =======\n\n";
	print_state pod_final;
	(if !do_mani then
		(printf "\n ======= BEST POD SOLUTION =======\n\n";
		printf "Best solution: ";
		print_int_list (snd best);
		printf "\nTrue beliefs: ";
		print_int_list (get_player_answers true_beliefs 1);
		printf "\nHamming-Distance for best solution: %d\n" (fst best);
		printf "Hamming-Distance for true beliefs: %d\n" h_true)
	else
		())

(* Print the result from the PWOD process *)
let print_pwod state best true_beliefs h_true =
	printf "\n\n ======= PWOD DIFFUSION =======\n\n";
	print_state state;
	(if !do_mani then
		(printf "\n ======= BEST PWOD SOLUTION =======\n\n";
		printf "Best solution: ";
		print_int_list (snd best);
		printf "\nTrue beliefs: ";
		print_int_list (get_player_answers true_beliefs 1);
		printf "\nHamming-Distance for best solution: %d\n" (fst best);
		printf "Hamming-Distance for true beliefs: %d\n" h_true)
	else 
		())