REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean: clean
	rm -rf _build

test:
	@$(REBAR) eunit --cover
	@$(REBAR) cover --verbose

xref:
	@$(REBAR) xref

dialyzer:
	@$(REBAR) dialyzer

.PHONY: compile dialyzer clean distclean test cover xref
