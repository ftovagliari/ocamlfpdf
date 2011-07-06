LIBDIR=fpdf
OCAMLLIB=`ocamlc -where`
INSTALL_PATH=$(OCAMLLIB)/$(LIBDIR)
INCLUDES=-I,+xml-light
LIBS=-libs unix,str,nums,cryptokit,xml-light

all:
	ocamlbuild -cflags $(INCLUDES) fpdf.cma fpdf.cmxa

doc: all
	mkdir -p ../doc
	cd _build && ocamldoc -d ../../doc *.mli -t "ocamlfpdf" -html

test-without-install: clean
	ocamlbuild -cflags $(INCLUDES) -lflags -I,+xml-light $(LIBS) test.byte
	_build/test.byte
	xpdf _build/test.byte.pdf

test: test-without-install
	ocamlc -o test -I +$(LIBDIR) -I +xml-light str.cma unix.cma nums.cma cryptokit.cma xml-light.cma fpdf.cma test.ml
	./test
	xpdf test.pdf

install:
	rm -f _build/test*
	mkdir -p $(INSTALL_PATH)
	cp -v *.mli $(INSTALL_PATH)
	cp -v _build/*.cmi $(INSTALL_PATH)
	cp -v _build/*.cma $(INSTALL_PATH)
	cp -v _build/*.cmxa $(INSTALL_PATH)

clean:
	rm -f *.cm* test.byte test*.pdf test
	rm -fr _build
	rm -f ../doc/*


