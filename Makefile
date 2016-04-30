PACKAGES=tyxml.functor react reactiveData js_of_ocaml js_of_ocaml.tyxml js_of_ocaml.syntax deriving lwt.ppx js_of_ocaml.deriving.ppx js_of_ocaml.ppx lwt

all: cv.byte
	js_of_ocaml +js_of_ocaml.weak/weak.js cv.byte

cv.byte: cv.ml
	ocamlfind ocamlc  \
		${addprefix -package , ${PACKAGES}} \
	        -linkpkg -o cv.byte cv.ml
clean:
	rm -f cv.byte cv.cmi cv.cmo cv.js *~
