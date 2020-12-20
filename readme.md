# aoc-2020

https://adventofcode.com/2020, `ocaml` style!

## setup

- `opam switch create aoc ocaml-system.4.10.0`
- `opam switch import opam.deps` (generated via `opam switch export ./opam.deps`)
- `rad run_day_X`
  - or `cp fixture.ml dayX/ && cd dayX && dune exec ./bin.exe ./input.txt`

You may also need to install `libgplk`, e.g. `brew install glpk`
