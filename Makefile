PROGRAM = cpc
OCB = ocamlbuild
ORUN = ocamlrun

all : byte

test: byte
	$(ORUN) $(PROGRAM).byte -t

clean:
	$(OCB) -clean

native:
	$(OCB) $(PROGRAM).native

byte:
	$(OCB) $(PROGRAM).byte
