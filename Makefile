SOURCES=\
	actor.ml \
	test1.ml \
	consensus.ml

TARGETS=\
	test1 \
	consensus

LIBS=unix.cma
OBJECTS=actor.cmo

%.cmi %.cmo:	%.ml
	ocamlc -c $<

%:	%.cmo
	ocamlc -o $@ $(LIBS) $(OBJECTS) $<

all:	$(TARGETS)

$(TARGETS):	$(OBJECTS)

clean:
	rm *.cmi *.cmo $(TARGETS) *.pdf *.dot *.svg *.png

dep:
	ocamldep $(SOURCES) > .depend.dev

-include .depend.dev
