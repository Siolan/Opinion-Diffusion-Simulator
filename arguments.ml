let verbose = ref false
let do_pod = ref true
let do_pwod = ref true
let do_mani = ref true
let input_file = ref "input"

(* True if computing best-response -> disable printing substate *)
let in_manipulation = ref false