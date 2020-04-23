# Rummikub

A terminal based Rummikub

## Preriquisities

- Dune
- Ocaml

##  Wanna try?

- If Dune is installed:

```bash
cd path/to/cloned_directory
dune build src/rummikub.ml
dune exec src/rummikub.exe # here .exe doesn't mean windows executable 
```

- On turing:

```bash
# We need a package manager to install dune
# Since we don't have opam, and we can't install
# it from source in turing, we will use brew.
# installation of brew: from https://brew.sh/ 
 
/bin/bash -c "export HAVE_SUDO_ACCESS=1 &&  $(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"


echo 'eval $(~/.linuxbrew/bin/brew shellenv)' >> ~/.bash_profile

source ~/.bash_profile

brew install dune

# now we can run Rummikub game:

cd path/to/cloned_directory
dune build src/rummikub.ml
dune exec src/rummikub.exe
```
