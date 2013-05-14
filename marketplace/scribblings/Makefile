all: out

out:
	raco scribble \
		--htmls \
		--dest out \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		marketplace.scrbl

clean:
	rm -rf out