PROG = prog
SOURCES = helpers.ml auction.ml driver.ml
OBJECTS = $(SOURCES:.ml=.cmo)

CAMLC = ocamlc

.PHONY: build run clean
.DEFAULT: build

%.cmo : %.ml
	$(CAMLC) -c $<

build: clean $(PROG)

$(PROG): $(OBJECTS)
	$(CAMLC) -o $(PROG) $(OBJECTS)

run: build
	@./$(PROG)

clean:
	rm -r *.cmo *.cmi $(PROG)

