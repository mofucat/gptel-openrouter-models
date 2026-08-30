EMACS ?= emacs
EL     = gptel-openrouter-models.el
TEST   = gptel-openrouter-models-test.el

.PHONY: all test compile clean

all: compile test

## Run the ERT suite (no network, gptel stubbed if absent).
test:
	$(EMACS) -Q --batch -l $(TEST) -f ert-run-tests-batch-and-exit

## Byte-compile, treating warnings as errors.
compile:
	$(EMACS) -Q --batch \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  --eval "(unless (require 'gptel nil t) (defvar gptel-model nil) (provide 'gptel))" \
	  -f batch-byte-compile $(EL)

clean:
	rm -f *.elc
