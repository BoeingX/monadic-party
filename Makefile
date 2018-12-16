.PHONY: all build slides clean run

all: build slides

build: src/ app/
	stack build

slides: main.pdf

main.pdf: src/Lib.lhs
	xelatex --shell-escape $<

clean:
	stack clean
