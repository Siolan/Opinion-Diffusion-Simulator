open Printf

(* Type for a propositional formula *)
type prop = 
  TOP
| BOT
| VAR of int
| CONJ of prop * prop
| DISJ of prop * prop
| IMP of prop * prop
| EQUIV of prop * prop
| XOR of prop * prop
| NOT of prop

(* Consider a list a binary number 0 and 1 as a binary number, this function add 1 to the binary number *)
let list_bin_plus l =
	let rec list_bin_plus_aux l res num =
		match l with
		| [] -> res
		| h :: t -> (match (h, num) with
			| (1, 1) -> list_bin_plus_aux t ([0] @ res) 1
			| (0, 1) -> list_bin_plus_aux t ([1] @ res) 0
			| _ -> list_bin_plus_aux t ([h] @ res) 0)
	in
	list_bin_plus_aux (List.rev l) [] 1

(* Create a list of size nb_question fill with num *)
let create_list_of num nb_question =
	let rec create_list_of_aux num res final =
		match List.length res with
		| a when a = final -> res
		| _ -> create_list_of_aux num ([num] @ res) final
	in
	create_list_of_aux num [] nb_question

(* Create a list of all possible profiles *)
let create_possibilities_list nb_question =
	let rec create_possibilities_list_aux res final =
		match List.hd res with
		| f when f = final -> res
		| f -> create_possibilities_list_aux ([(list_bin_plus f)] @ res) final
	in 
	create_possibilities_list_aux ([create_list_of 0 nb_question]) (create_list_of 1 nb_question)

(* Get the element i in a given list l *)
let get_answer_ic i l =
	let rec get_answer_ic_aux i l curr =
		match l with
		| [] -> failwith("Question index out of bounds")
		| h :: t when i = curr -> h
		| h :: t -> get_answer_ic_aux i t (curr + 1)
	in 
	get_answer_ic_aux i l 0

(* In a int list list, inverse the question given (0 gives 1 and 1 gives 0) *)
let inverse_question question l =
	List.map (
		fun a -> List.mapi (
			fun index q -> match (index, q) with
				| (i, b) when i = question -> 1 - b
				| (i, b) -> b
	) a) l

(* Returns true if the given element i is in the list l *)
let rec appartient_list i l =
	match l with
	| [] -> false
	| h :: t when h = i -> true
	| h :: t -> appartient_list i t

(* Given l a int list list, returns a int list list in which every element is present once *)
let suppression_double l =
	let rec suppression_double_aux l res = 
		match l with
		| [] -> res
		| h :: t -> if appartient_list h res then 
				suppression_double_aux t res 
			else 
				suppression_double_aux t (res @ [h])
	in
	suppression_double_aux l []

(* Create the integrity set of possiles profiles given a propositional formula by creating all possibilities and delete
   each that does not satisfy the formula *)
let create_ic proposition nb_question =
	let rec create_ic_aux proposition res =
		match proposition with
		| TOP -> res
		| BOT -> []
		| VAR a -> List.filter (fun i -> get_answer_ic a i = 1) res
		| CONJ (a, b) -> create_ic_aux a (create_ic_aux b res)
		| DISJ (a, b) -> suppression_double ((create_ic_aux a res) @ (create_ic_aux b res))
		| IMP (a, b) -> create_ic_aux (DISJ ((NOT a), b)) res
		| EQUIV (a, b) -> create_ic_aux (CONJ (IMP (a, b), IMP (b, a))) res
		| XOR (a, b) -> create_ic_aux (DISJ (CONJ (a, NOT b), CONJ (b, NOT a))) res
		| NOT a -> (match a with
			| TOP -> []
			| BOT -> res
			| VAR b -> inverse_question b (create_ic_aux (VAR b) (inverse_question b res))
			| CONJ (b, c) -> create_ic_aux (DISJ (NOT b, NOT c)) res
			| DISJ (b, c) -> create_ic_aux (CONJ (NOT b, NOT c)) res
			| IMP (b, c) -> create_ic_aux (CONJ (b, NOT c)) res
			| EQUIV (b, c) -> create_ic_aux (DISJ (CONJ (b, NOT c), CONJ(c, NOT b))) res
			| XOR (b, c) -> create_ic_aux (CONJ (DISJ (NOT b, c), DISJ (NOT c, b))) res
			| NOT b -> create_ic_aux b res)
	in
	create_ic_aux proposition (create_possibilities_list nb_question)