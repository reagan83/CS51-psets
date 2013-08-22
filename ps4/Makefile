all: ps4.ml
	ocamlc -g -o ps4 ps4.ml
	ocamlc -g -o modules modules.ml

ps4: ps4.ml
	ocamlc -g -o ps4 ps4.ml

modules: modules.ml
	ocamlc -g -o modules modules.ml

clean:
	rm -f modules ps4 *.cmi *.cmo
