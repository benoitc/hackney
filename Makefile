REBAR?=rebar
DIALYZER?=dialyzer
PLT=.hackney.plt

ERLANG_APPS=asn1 compiler crypto edoc erts inets kernel mnesia public_key ssl stdlib syntax_tools tools xmerl
DEPS=$(notdir $(wildcard deps/*))
DIALYZER_DEPS=$(foreach dep,$(DEPS),deps/$(dep)/ebin)

all: build

dev: devbuild

doc: dev
	$(REBAR) -C rebar_dev.config doc

clean:
	$(REBAR) clean

distclean: clean
	@rm -rf deps

build: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

test:
	$(REBAR) skip_deps=true eunit

# development
#
devclean:
	$(REBAR) -C rebar_dev.config clean

devbuild: devdeps
	$(REBAR) -C rebar_dev.config compile

devdeps:
	$(REBAR) -C rebar_dev.config get-deps

dialyzer: $(PLT) devbuild
	$(DIALYZER) --plts $(PLT) -r ebin

$(PLT):
	$(DIALYZER) --build_plt $(DIALYZER_DEPS) --apps $(ERLANG_APPS) --output_plt $(PLT)

# Mimetypes module generator.

GEN_URL = http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
GEN_FILE = mime.types
GEN_SRC = src/hackney_lib/hackney_mimetypes.erl.src
GEN_OUT = src/hackney_lib/hackney_mimetypes.erl

gen:
	@wget -qO $(GEN_FILE) $(GEN_URL)
	@cat $(GEN_SRC) \
		| head -n `grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		> $(GEN_OUT)
	@cat $(GEN_FILE) \
		| grep -v ^# \
		| awk '{for (i=2; i<=NF; i++) if ($$i != "") { \
			print "extensions(<<\"" $$i "\">>) -> <<\"" $$1 "\">>;"}}' \
		| sort \
		| uniq -w 25 \
		>> $(GEN_OUT)
	@echo "extensions(_) -> <<\"application/octet-stream\">>." >> $(GEN_OUT)
	@echo "" >> $(GEN_OUT)
	@cat $(GEN_FILE) \
		| grep -v ^# \
		| awk '{\
			printf("mimetypes(<<\"%s\">>) -> [", $$1); \
			for (i=2; i<=NF; i++) \
				if ($$i != "") { \
					if (i >= 3){printf(",")} \
					printf("<<\"%s\">>",  $$i) \
				}\
			print "];" \
			}' \
		| sort \
		>> $(GEN_OUT)
	@echo "mimetypes(_) -> [<<>>]." >> $(GEN_OUT)
	@cat $(GEN_SRC) \
		| tail -n +`grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		>> $(GEN_OUT)

.PHONY: doc deps test
