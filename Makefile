REBAR?=./rebar3
DIALYZER?=dialyzer
PLT=.hackney.plt

all: build

doc:
	$(REBAR) edoc

clean:
	$(REBAR) clean

build:
	$(REBAR) compile

test:
	$(REBAR) eunit

dialyzer:
	$(REBAR) dialyzer

distclean:
	@rm -rf _build

.PHONY: doc test
