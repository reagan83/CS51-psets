all: ps6

FILES=music.ml refs.ml streams.ml

ps6: $(FILES)
	@echo "Compiling..."
	ocamlc -g -o streams unix.cma str.cma streams.ml
	ocamlc -g -o refs unix.cma str.cma refs.ml
	ocamlc -g -o music unix.cma str.cma music.ml

check: $(FILES)
	chmod u+x ./check_width
	./check_width refs.ml
	./check_width streams.ml
	./check_width music.ml

run: ps6
	@echo "Running..."
# On windows, this needs to be *.exe, or you can run it manually.
	./streams
	./refs
	./music
	@echo "done!  Hopefully there were no errors.\n"

submit: run
	@echo "Submitting..."
	chmod u+x ./submit.sh
	./submit.sh

clean: 
	rm -f ps6 *.cmi *.cmo
