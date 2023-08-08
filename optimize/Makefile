# Makefile for binding optimizer:

# replacement stuff
empty     :=
space     := $(empty) $(empty)

# The load path of the application:
LOADPATH = src:$(subst $(space),:,$(wildcard .cpm/packages/*/src))

# Source modules:
DEPS = .cpm/packages/*/src/*/*curry src/BindingOpt.curry

.PHONY: all
all: install

.PHONY: install
install: compile

.PHONY: compile
compile: BindingOpt

# Load the application into the interactive Curry system:
.PHONY: load
load: 
	$(REPL) -Dbindingoptimization=no --nocypm $(REPL_OPTS) :set path $(LOADPATH) :l BindingOpt

# Compile and create an executable of the binding optimizer:
BindingOpt: $(DEPS)
	$(REPL) -Dbindingoptimization=no --nocypm $(REPL_OPTS) :set path $(LOADPATH) :l BindingOpt :save :q

# Clean intermediate files:
.PHONY: clean
clean: 
	/bin/rm -rf src/.curry .cpm/packages/*/src/.curry

.PHONY: uninstall
uninstall: clean
	rm -f BindingOpt

# run the test suite
runtest:
	$(BINDIR)/cypm -d curry_bin=$(REPL) test
