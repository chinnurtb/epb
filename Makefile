.PHONY: all clean compile eunit test qc

all: compile

clean:
	@rebar clean

compile:
	@rebar compile

qc: compile
	@rebar qc skip_deps=true

eunit: compile
	@rebar eunit skip_deps=true

test: eunit qc
