# Rummikub

A terminal based Rummikub. A work that uses the Library [ANSITerminal](https://github.com/Chris00/ANSITerminal)

## Preriquisities

- Dune
- Ocaml

##  Wanna try?

- If Dune is installed:

```bash
cd path/to/cloned_directory
make test
```

- On turing:

```bash
# We need a package manager to install dune
# Since we don't have opam, and we can't install
# it from source on turing, we will use brew.
# installation of brew: from https://brew.sh/ 
 
/bin/bash -c "export HAVE_SUDO_ACCESS=1 &&  $(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

echo 'eval $(~/.linuxbrew/bin/brew shellenv)' >> ~/.bash_profile
source ~/.bash_profile
brew install dune

# now we can run Rummikub game:

cd path/to/cloned_directory
make test
```
