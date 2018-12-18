# Monadic-party
[![Build Status](https://travis-ci.org/BoeingX/monadic-party.svg?branch=master)](https://travis-ci.org/BoeingX/monadic-party)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


## Dependencies

Since `LaTeX` and `Haskell` are involved, you will need a working environment for both languages.
It is recommended to setup [TeX Live](https://www.tug.org/texlive/) for LaTeX
and [Stack](https://docs.haskellstack.org/en/stable/README/) for Haskell.

## Get started

```bash
$ stack build                       # build Haskell project
$ xelatex -shell-escape src/Lib.lhs # compile beamer slides
```
