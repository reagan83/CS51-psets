# This program

PROG = bees

# Setup

LIBS = \
	graphics.cma

CAMLC = ocamlc
CAMLDOC = ocamldoc

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files

SOURCES = \
	Direction.ml Draw.ml Helpers.ml \
	Event.ml \
	WorldObjectI.ml World.ml WorldObject.ml \
	Movable.ml \
	Ageable.ml Dust.ml CarbonBased.ml \
	Pond.ml Flower.ml \
	Bee.ml BeeBouncy.ml BeeRandom.ml Hive.ml \
	Bear.ml Cave.ml \
  Cow.ml Pasture.ml \
	UI.ml \

OBJECTS = $(SOURCES:.ml=.cmo)

# Basic Program

$(PROG)_basic: $(OBJECTS) MainBasic.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) MainBasic.cmo -o $(PROG)_basic

build_basic: $(PROG)_basic

run_basic: build_basic
	@./$(PROG)_basic

# Movement Program

$(PROG)_movement: $(OBJECTS) MainMovement.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) MainMovement.cmo -o $(PROG)_movement

build_movement: $(PROG)_movement

run_movement: build_movement
	@./$(PROG)_movement

# Actions Program

$(PROG)_actions: $(OBJECTS) MainActions.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) MainActions.cmo -o $(PROG)_actions

build_actions: $(PROG)_actions

run_actions: build_actions
	@./$(PROG)_actions

# Aging Program

$(PROG)_aging: $(OBJECTS) MainAging.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) MainAging.cmo -o $(PROG)_aging

build_aging: $(PROG)_aging

run_aging: build_aging
	@./$(PROG)_aging

# Smart Program

$(PROG)_smart: $(OBJECTS) MainSmartBees.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) MainSmartBees.cmo -o $(PROG)_smart

build_smart: $(PROG)_smart

run_smart: build_smart
	@./$(PROG)_smart

# Final Program

$(PROG)_final: $(OBJECTS) MainFinal.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) MainFinal.cmo -o $(PROG)_final

build_final: $(PROG)_final

run_final: build_final
	@./$(PROG)_final

# DocGen

doc_final: $(FINAL_OBJECTS)
	$(CAMLDOC) -html $(FINAL_SOURCES)

# Defaults

build: build_final
run: run_final

# Other

all: build_basic build_movement build_final

clean:
	rm *.cmo *.cmi *.html *.css \
	$(PROG)_basic $(PROG)_movement $(PROG)_final $(PROG)_actions \
	$(PROG)_aging $(PROG)_smart $(PROG)_final \
	|| echo

.DEFAULT_GOAL := build
.PHONY: doc build run clean
