PACKAGES=tyxml.functor react reactiveData js_of_ocaml js_of_ocaml.tyxml js_of_ocaml.syntax deriving lwt_ppx js_of_ocaml-ppx_deriving_json js_of_ocaml.ppx lwt js_of_ocaml-lwt

all: cv.byte
	js_of_ocaml cv.byte
cv.byte: cv.ml
	ocamlfind ocamlc  \
		${addprefix -package , ${PACKAGES}} \
	        -linkpkg -o cv.byte cv.ml
clean:
	rm -f cv.byte cv.cmi cv.cmo cv.js cv.html *~

install_libs:
	opam install js_of_ocaml tyxml react lwt reactiveData deriving\
		ppx_tools js_of_ocaml-tyxml \
		lwt_ppx js_of_ocaml-ppx_deriving_json js_of_ocaml-lwt
