build:
	dune build src/rummikub.ml

test: build
	dune exec src/rummikub.exe

clean:
	dune clean

