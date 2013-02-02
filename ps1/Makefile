all: ps1.ml
	ocamlc -o ps1 ps1.ml

submit: ps1.ml
	chmod 700 ./submit.sh
	./submit.sh

clean:
	rm -f *.cmo *.cmi ps1
