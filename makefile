.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: install
install:
	stack install


.PHONY: test
test:
	stack test


.PHONY: docs
docs:
	stack haddock --haddock-for-hackage


.PHONY: release
release: docs
	stack upload .
	stack upload --documentation .
