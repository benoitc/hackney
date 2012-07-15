REBAR?=./rebar

all: build

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean

distclean: clean
	@rm -rf deps

build: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps


.PHONY: doc deps
