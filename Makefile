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

# h2spec HTTP/2 compliance testing
H2SPEC_VERSION ?= 2.6.0

priv/h2spec:
	@mkdir -p priv
	@echo "Downloading h2spec v$(H2SPEC_VERSION)..."
	@OS=$$(uname -s | tr '[:upper:]' '[:lower:]'); \
	ARCH=$$(uname -m); \
	if [ "$$ARCH" = "x86_64" ]; then ARCH="amd64"; fi; \
	if [ "$$ARCH" = "aarch64" ] || [ "$$ARCH" = "arm64" ]; then \
		if [ "$$OS" = "darwin" ]; then ARCH="amd64"; else ARCH="arm64"; fi; \
	fi; \
	curl -sL "https://github.com/summerwind/h2spec/releases/download/v$(H2SPEC_VERSION)/h2spec_$${OS}_$${ARCH}.tar.gz" | \
	tar xz -C priv

download-h2spec: priv/h2spec
	@echo "h2spec installed at priv/h2spec"

h2spec-test: priv/h2spec compile
	@${REBAR} ct --suite=h2spec_SUITE

e2e-test: compile
	@${REBAR} ct --suite=hackney_http2_e2e_SUITE

http2-bench: compile
	@${REBAR} as test compile
	@erl -pa _build/test/lib/*/ebin -pa _build/test/lib/hackney/test -noshell \
		-eval 'hackney_http2_machine_bench:run().' -s init stop
