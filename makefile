DIALYZER_OPTS = -Wunderspecs

ERLANG_DIALYZER_APPS = asn1 compiler crypto \
                       edoc erts eunit gs hipe \
                       inets kernel mnesia observer \
                       public_key runtime_tools ssl \
                       stdlib syntax_tools tools \
                       webtool xmerl

all: compile eunit dialyzer

# Clean ebin and .eunit of this project
clean:
	@rebar clean

compile:
	@rebar compile

eunit:
	@rebar skip_deps=true eunit

test: eunit dialyzer

dialyzer: ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) -r ebin

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	@rebar doc skip_deps=true

.PHONY: all compile eunit test dialyzer clean doc
