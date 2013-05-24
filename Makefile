APP=erstar
REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)

.PHONY: test

all: compile

get-deps:
	$(REBAR) get-deps

compile: get-deps
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

clean:
	$(REBAR) clean
	rm -rfv erl_crash.dump

clean-app:
	$(REBAR) clean skip_deps=true
	rm -rfv erl_crash.dump

distclean: clean
	rm -rfv ebin deps logs

test:
	mkdir -p .eunit
	$(REBAR) eunit skip_deps=true -v || true

dialyzer:
	dialyzer ebin -Wrace_conditions -Wunderspecs -Werror_handling

ci: compile test

doc:
	$(REBAR) -C docs.config get-deps compile doc
