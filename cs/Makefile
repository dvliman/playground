shell:
	@rm -f src/*.beam
	@rm -f test/*.beam
	rebar co skip_deps=true
	erl -pa ebin -pa deps/*/ebin -s lager -s cs -config etc/sys.config

test:
	@rm -f src/*.beam
	@rm -f test/*.beam
	rebar co skip_deps=true ct
	@open logs/index.html
	@rm -f test/*.beam

.PHONY: all test clean
