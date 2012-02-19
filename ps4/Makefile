all: ps4.ml
	ocamlc -g -o ps4 ps4.ml

submit: ps4.ml
	chmod u+x ../submit.sh
	../submit.sh 4

clean: 
	rm -f ps4 *.cmi *.cmo
