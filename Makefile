CAMLJOBS=arguments.ml parser.ml players.ml aggregation_functions.ml ic.ml custom_print.ml pod.ml pwod.ml manipulation.ml main.ml

all: sym_mani

sym_mani: $(CAMLJOBS)
	ocamllex lex_prop.mll
	ocamlyacc grammar_prop.mly
	ocamlc -c ic.ml
	ocamlc -c grammar_prop.mli
	ocamlc -c lex_prop.ml
	ocamlc -g -o sym_manipulation str.cma grammar_prop.ml lex_prop.ml $(CAMLJOBS)

clean:
	rm -f sym_manipulation *.cmi *.cmo
	rm -f lex_prop.ml lex_prop.mli grammar_prop.ml grammar_prop.mli
