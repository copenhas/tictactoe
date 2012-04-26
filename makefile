
build:
	rebar compile

shell: build
	erl -pa ebin -I include

clean:
	rebar clean
