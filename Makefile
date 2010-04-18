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
