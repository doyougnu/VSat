#! /usr/bin/env sh
git clone https://github.com/Z3Prover/z3.git
cd z3
python3 scripts/mk_make.py
cd build
make
make install
