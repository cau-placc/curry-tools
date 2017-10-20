# Makefile for Curry Package Manager (CPM)


# The tool name of the application:
TOOL = $(BINDIR)/cypm

# The default options for the REPL (options "rts -T" required for KiCS2
# in order to get elapsed times):
ifeq ($(CURRYSYSTEM),kics2)
export REPL_OPTS = --noreadline :set -time :set rts -T
else
export REPL_OPTS = --noreadline :set -time
endif

# Source modules of CPM:
DEPS = src/CPM/*.curry src/CPM/*/*.curry

.PHONY: all compile install clean uninstall runtest

all: install

install: compile
	rm -f $(TOOL)
	cd $(BINDIR) && ln -s ../currytools/cpm/src/CPM.Main $(notdir $(TOOL))

compile: src/CPM.Main

clean:
	rm -Rf src/CPM.Main src/.curry vendor/*/src/.curry

uninstall: clean
	rm -f $(TOOL)

src/CPM.Main: src/CPM/ConfigPackage.curry $(DEPS)
	@echo Root location of Curry system: $(ROOT)
	@if [ ! -d "$(ROOT)" ] ; then echo "Error: not a valid directory!" ; exit 1; fi
	@export CURRYPATH="";						\
	for i in `ls vendor`; do 					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src"; 	\
	done; 								\
	echo "Set CURRYPATH to $$CURRYPATH"; 				\
	cd src; $(REPL) $(REPL_OPTS) :l CPM.Main :save :quit

src/CPM/ConfigPackage.curry: Makefile
	@echo "module CPM.ConfigPackage where" > $@
	@echo "packagePath :: String" >> $@
	@echo "packagePath = \"$(CURDIR)\"" >> $@
	@echo "packageVersion :: String" >> $@
	@echo "packageVersion = \"2.0.0\"" >> $@
	@echo "Curry configuration module '$@' written."

runtest:
	@export CURRYPATH="";						\
	for i in `ls vendor`; do					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src";	\
	done;								\
	cd src; $(REPL) check CPM.Package CPM.Resolution CPM.LookupSet

.PHONY: doc
doc:
	@export CURRYPATH="";						\
	for i in `ls vendor`; do					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src";	\
	done;								\
	export CURRYPATH="$$CURRYPATH:`pwd`/src"; \
	$(REPL) doc cdoc CPM.Main

.PHONY: manual
manual:
	pdflatex -output-directory=docs docs/manual.tex
