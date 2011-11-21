LIBDIR=fpdf
OCAMLLIB=`ocamlc -where`
INSTALL_PATH=$(OCAMLLIB)/$(LIBDIR)
INCLUDES=-I,+xml-light,-I,+lablgtk2
LIBS=-libs unix,str,xml-light

all: 
	ocamlbuild -cflags $(INCLUDES) fpdf.cma fpdf.cmxa

gtk_pdfmarkup_editor: 
	ocamlbuild -cflags $(INCLUDES),-w,sy $@.cma $@.cmxa

test_pdfmarkup_editor: gtk_pdfmarkup_editor
	cd _build && ocamlc -o $@ -I +lablgtk2 unix.cma str.cma lablgtk.cma gtk_pdfmarkup_editor.cma gtkInit.cmo ../$@.ml
	_build/$@

doc: all
	mkdir -p ../doc
	cd _build && ocamldoc -d ../../doc *.mli -t "ocamlfpdf" -html

test-without-install: clean
	ocamlbuild -cflags $(INCLUDES) -lflags -I,+xml-light $(LIBS) test.byte
	_build/test.byte
	xpdf _build/test.byte.pdf

test: test-without-install
	#
	# Now testing with the installed library...
	#
	ocamlc.opt -o test -I +$(LIBDIR) -I +xml-light str.cma unix.cma xml-light.cma fpdf.cma test.ml
	./test
	xpdf test.pdf

test.opt:
	ocamlopt.opt -o test -I +$(LIBDIR) -I +xml-light str.cmxa unix.cmxa xml-light.cmxa fpdf.cmxa test.ml
	./test
	xpdf test.pdf

install:
	rm -f _build/test*
	mkdir -p $(INSTALL_PATH)
	cp -v *.mli $(INSTALL_PATH)
	cp -v _build/*.cmi $(INSTALL_PATH)
	cp -v _build/*.cma $(INSTALL_PATH)
	cp -v _build/*.cmxa $(INSTALL_PATH)
	cp -v _build/*.a $(INSTALL_PATH)

clean:
	rm -f *.cm* test.byte test*.pdf test test.opt test.byte *.o *.a *.annot
	rm -fr _build
	rm -f ../doc/*
	ocamlbuild -clean


