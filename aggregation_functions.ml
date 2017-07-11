open Players

(* The strict majority function *)
let strict_maj graphe player state question =
	(* Get the answers of all the influencers on a question *)
	let rec strict_maj_aux inf state question res =
		match inf with
		| [] -> res
		| h :: t -> (*Printf.printf "Je lis %d chez %d pour la question %d\n" (get_answer state h question) h question;*)
			strict_maj_aux t state question (res @ [get_answer state h question])
	in 

	(* Count the number of 0 and 1 in the questions *)
	let rec count_0_and_1 l n0 n1 =
		match l with
		| [] -> (n0, n1)
		| h :: t when h = 1 -> count_0_and_1 t n0 (n1 + 1)
		| h :: t when h = 0 -> count_0_and_1 t (n0 + 1) n1
		| h :: t -> failwith ("Non binary answer for the question !")
	in

	match count_0_and_1 (strict_maj_aux (influencers graphe player) state question []) 0 0 with
	| (a, b) when a = b -> None 
	| (a, b) when a > b -> Some 0
	| (a, b) -> Some 1