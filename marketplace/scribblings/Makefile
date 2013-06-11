all: out

pages:
	@(git branch -v | grep -q gh-pages || (echo local gh-pages branch missing; false))
	@echo
	@git branch -av | grep gh-pages
	@echo
	@(echo 'Is the branch up to date? Press enter to continue.'; read dummy)
	git clone -b gh-pages ../.. pages

publish: out pages
	rm -rf pages/*
	cp -r out/marketplace/. pages/.
	(cd pages; git add -A)
	-(cd pages; git commit -m "Update $$(date +%Y%m%d%H%M%S)")
	(cd pages; git push)
	rm -rf pages

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
