APPLICATION := ehl7

ERL := erl
EPATH := -pa ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit

.PHONY: all clean compile console depclean deps distclean dialyze doc test test-compile

all: compile

clean:
	@rebar skip_deps=true clean

compile:
	@rebar compile

console:
	$(ERL) -sname $(APPLICATION) $(EPATH)

depclean:
	@rebar clean

deps:
	@rebar get-deps

dialyze: compile
	@dialyzer -r .

distclean:
	@rebar clean delete-deps

doc:
	@rebar skip_deps=true doc

test: compile
	@rebar skip_deps=true eunit

test-console:
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)
