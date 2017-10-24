# Makefile for generating GenInt tool

# binary
TOOL = $(BINDIR)/runcurry

# The load path of the browser:
LOADPATH = src

# Source modules:
DEPS = src/*.curry

.PHONY: all compile install clean uninstall runtest

all: install

compile: RunCurry

install: compile
	rm -f $(TOOL) $(BINDIR)/runcurry
	cd $(BINDIR) && ln -s ../currytools/runcurry/RunCurry $(notdir $(TOOL))

clean:
	$(CLEANCURRY)
	rm -f RunCurry

uninstall: clean
	rm -f $(TOOL)

# generate executable for currydoc program:
RunCurry: $(DEPS)
	$(REPL) --nocypm $(REPL_OPTS) :set path $(LOADPATH) :load RunCurry :save :quit

# run the test suite
runtest:
	$(BINDIR)/cypm -d curry_bin=$(REPL) test
