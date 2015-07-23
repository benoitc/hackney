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

# CA generation
#
CA_BUNDLE_BIN=./support/mk-ca-bundle.pl
CA_BUNDLE=ca-bundle.crt
CA_SRC=src/hackney_connect/hackney_cacerts.erl.src
CA_OUT=src/hackney_connect/hackney_cacerts.erl

mkcert:
	$(CA_BUNDLE_BIN)
	@cat $(CA_SRC) \
		| head -n `grep -n "%% GENERATED" $(CA_SRC) | cut -d : -f 1` \
		> $(CA_OUT)
	@cat $(CA_BUNDLE) >> $(CA_OUT)
	@cat $(CA_SRC) \
		| tail -n +`grep -n "%% GENERATED" $(CA_SRC) | cut -d : -f 1`  \
		>> $(CA_OUT)
	mv $(CA_BUNDLE) priv/


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
