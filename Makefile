COLLECTIONS=marketplace

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf

setup:
	raco setup $(COLLECTIONS)

link:
	raco pkg install --link $$(pwd)

unlink:
	raco pkg remove $$(basename $$(pwd))
