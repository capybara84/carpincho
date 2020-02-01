PROGRAM = cpc
OCB = ocamlbuild
ORUN = ocamlrun

all : byte

run: byte
	$(ORUN) $(PROGRAM).byte

clean:
	$(OCB) -clean

native:
	$(OCB) $(PROGRAM).native

byte:
	$(OCB) $(PROGRAM).byte
