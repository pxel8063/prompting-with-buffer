## -*- mode: makefile-gmake -*-

# set up environment
 include mk/default.mk	# defaults, customizable via "local.mk"
-include local.mk	# optional local customization, use default.mk as template

BATCH = $(EMACS) -Q -batch -L . -L test

el = pwb.el
test =  test/pwb-test.el

compile: $(el:.el=.elc) $(test:.el=.elc)

%.elc: %.el
	@$(info Compiling file $<)
	@$(BATCH) -f batch-byte-compile $<

test/pwb-test.elc: pwb.elc

test: $(el:.el=.elc) $(test:.el=.elc)
	@$(BATCH) -l test/pwb-test.elc -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	$(RM) $(el:.el=.elc) $(test:.el=.elc)
