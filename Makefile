PROGRAM = cpc
OCB = ocamlbuild

all : byte

clean:
	$(OCB) -clean

native:
	$(OCB) $(PROGRAM).native

byte:
	$(OCB) $(PROGRAM).byte
