#****************************************************************************
# Makefile for Curry preprocessor
#****************************************************************************

# Tool binary
TOOL = $(BINDIR)/currypp

# replacement stuff
empty     :=
space     := $(empty) $(empty)

# The load path of the browser:
LOADPATH = src:src/IntegratedCode:src/IntegratedCode/Parser:src/IntegratedCode/Parser/ML:src/IntegratedCode/Parser/SQL:src/SequentialRules:src/DefaultRules:src/ContractWrapper:$(subst $(space),:,$(wildcard .cpm/packages/*/src))

# Source modules:
DEPS = .cpm/packages/*/src/*/*curry\
       src/*.curry src/*/*.curry src/*/*/*.curry src/*/*/*/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: Main

install: compile
	rm -f $(TOOL)
	cd $(BINDIR) && ln -s ../currytools/currypp/Main $(notdir $(TOOL))

clean:
	$(CLEANCURRY) -r
	rm -f Main

uninstall: clean
	rm -f $(TOOL)

# generate executable of currypp translator:
Main: $(DEPS)
	@echo Compiling Curry Preprocessor...
	$(REPL) --nocypm $(REPL_OPTS) :set path $(LOADPATH) :load Main :save :quit


########################################################################
# Testing the preprocessor via CPM
.PHONY: runtest
runtest:
	$(BINDIR)/cypm -d curry_bin=$(REPL) test
