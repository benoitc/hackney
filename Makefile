REBAR ?= $(shell which rebar3 2>/dev/null)

.PHONY: all compile test clean distclean dialyzer doc

all: compile

compile:
	@${REBAR} compile

test: compile
	@${REBAR} eunit

dialyzer: compile
	@${REBAR} dialyzer

clean:
	@${REBAR} clean

distclean: clean
	@rm -rf _build
	@rm -rf priv/*.so priv/*.dll
	@git clean -fdx -e "*.plt"

doc:
	@${REBAR} ex_doc

# QUIC-specific targets
quic-deps:
	@echo "Vendoring QUIC dependencies..."
	@test -d libs/ngtcp2 || git subtree add --prefix=libs/ngtcp2 https://github.com/ngtcp2/ngtcp2.git v1.18.0 --squash
	@test -d libs/nghttp3 || git subtree add --prefix=libs/nghttp3 https://github.com/ngtcp2/nghttp3.git v1.9.0 --squash

quic-deps-update:
	@echo "Updating QUIC dependencies..."
	git subtree pull --prefix=libs/ngtcp2 https://github.com/ngtcp2/ngtcp2.git v1.18.0 --squash
	git subtree pull --prefix=libs/nghttp3 https://github.com/ngtcp2/nghttp3.git v1.9.0 --squash

quic-clean:
	@rm -rf _build/cmake
	@rm -f priv/hackney_quic.so priv/hackney_quic.dll
