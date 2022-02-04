STACK := $(shell command --version stack 2> /dev/null)

ifeq (, $(shell which stack))
	$(error "No stack in PATH, please install or add to path")
endif

install:
	stack build

run: install
	stack exec cleaning-robot-hs-exe

unitest:
	stack test

all:  unitest exec
