ERLC=erlc

all: 
	$(ERLC) *.erl

clean: 
	rm -f *.beam
