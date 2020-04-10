REBAR ?= rebar3
PROJECT := conf
BUILD_IMAGE  ?= gitlab.bdt.tools:5000/build-ubuntu1804:1.4.2

.PHONY: compile clean distclean test xref dialyzer dialyze linter lint

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

dialyze:
	@$(REBAR) dialyzer

linter:
	@$(REBAR) as lint lint

lint:
	@$(REBAR) as lint lint

.PHONY: d_%

d_%:
	./build-with-env --image $(BUILD_IMAGE) make $*
