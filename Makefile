build:
	dune build src/rummikub.exe

test: build
	dune exec src/rummikub.exe

clean:
	dune clean

