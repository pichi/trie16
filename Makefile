all: erl

shell: erl
	@erl -pa ebin

erl: ebin
	@erl -make

ebin:
	@mkdir ebin

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam

dialyzer:
	@dialyzer -c ebin

test: erl
	@echo "testing:"
	@erl -noshell -pa ebin -eval 'eunit:test({dir,"ebin/"})' -s init stop
