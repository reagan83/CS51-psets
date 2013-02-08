all: expressiontop expression mapreduce
run: expressiontop run_exp_top

mapreduce: mapreduce.ml
	ocamlc -g -o mapreduce mapreduce.ml

LIB_FILES=ast.ml expressionLibrary.ml 
FILES=$(LIB_FILES) expression.ml

expression: $(FILES)
	ocamlc -g -o expression $(FILES)

expressiontop : $(FILES)
	ocamlmktop -o expressiontop $(FILES)

run_exp_top:
	./expressiontop

check: expression.ml  mapreduce.ml
	chmod u+x ./check_width
	./check_width mapreduce.ml
	./check_width expression.ml

submit: expression.ml  mapreduce.ml
	chmod u+x ./submit.sh
	./submit.sh

clean: 
	rm -f expression expressiontop mapreduce *.cmi *.cmo
