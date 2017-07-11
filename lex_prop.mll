{
	open Lexing
	open Grammar_prop
}

let var = ['0'-'9']+
let prop_and = "and" | "AND" | "And" | "&" | "&&" | "∧"
let prop_or = "or" | "OR" | "Or" | "|" | "||" | "∨"
let prop_not = "not" | "NOT" | "Not" | "!" | "¬"
let prop_top = "top" | "TOP" | "Top" | "T" | "true" | "True" | "T"
let prop_bot = "bot" | "BOT" | "Bot" | "F" | "false" | "FALSE" | "⊥"
let prop_imp = "imp" | "IMP" | "Imp" | "=>" | "⇒"
let prop_equiv = "equiv" | "EQUIV" | "Equiv" | "<=>" | "⟺"
let prop_xor = "xor" | "XOR" | "Xor" | "⊕"

rule token = parse
	| "("						{ LPAR }
	| ")"						{ RPAR }
	| prop_top 					{ T }
	| prop_bot					{ B }
	| prop_and					{ AND }
	| prop_or			 		{ OR }
	| prop_not					{ N }
	| prop_imp					{ I }
	| prop_equiv				{ E }
	| prop_xor					{ X }
	| var as v					{ V (int_of_string v) }
  	| [' ' '\t' '\r' '\n']+		{ token lexbuf }
	| eof						{ EOF }