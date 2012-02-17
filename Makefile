APPLICATION := ehl7

ERL := erl
EPATH := -pa ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit

.PHONY: all doc clean test

all: compile

compile:
	@rebar compile

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

distclean:
	@rebar delete-deps

dialyze: compile
	@dialyzer -r .

test:
	@rebar skip_deps=true eunit

console:
	$(ERL) -sname $(APPLICATION) $(EPATH)

test-console: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)
