
build: tags
	rebar compile

shell: build
	erl -pa ebin -I include

tags:
	ctags -R -f .tags

clean:
	rebar clean
