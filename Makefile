REBAR?=./rebar3
DIALYZER?=dialyzer
PLT=.hackney.plt

all: build

dev: devbuild

doc: dev
	$(REBAR) as dev edoc

clean:
	$(REBAR) clean

build:
	$(REBAR) compile

test:
	$(REBAR) eunit

# development
#
devclean:
	$(REBAR) as dev clean

devbuild:
	$(REBAR) as dev compile

dialyzer:
	$(REBAR) dialyzer
