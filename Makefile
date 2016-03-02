PROJECT = hackney
PROJECT_DESCRIPTION = Erlang HTTP client library
PROJECT_VERSION = 1.4.10

DEPS = idna mimerl certifi metrics ssl_verify_hostname

dep_idna = hex 1.1.0
dep_mimerl= hex 1.0.2
dep_certifi= hex 0.3.0
dep_metrics= hex 1.0.1
dep_ssl_verify_hostname= hex 1.0.5

DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}, \
			{top_level_readme, {"./README.md", "http://github.com/benoitc/hackney"}}


all:: deps app rel

doc: edoc

distclean:: distclean-edown

distclean-edown:
	rm -rf doc/*.md

app:: rebar.config

include erlang.mk
