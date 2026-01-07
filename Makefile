## -*- mode: makefile-gmake -*-

# set up environment
 include mk/default.mk	# defaults, customizable via "local.mk"
-include local.mk	# optional local customization, use default.mk as template

EMACSQ = $(EMACS) -Q

all: pwb-curl.elc

%.elc: %.el
	@$(info Compiling file $<)
	@$(EMACSQ) --batch -f batch-byte-compile $<

test: pwb-curl.el test/pwb-test.el
	@$(EMACSQ) -l pwb-curl.el -l test/pwb-test.el -batch -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	$(RM) pwb-curl.elc
