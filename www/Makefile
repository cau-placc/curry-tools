# Compile programs for managing cgi server processes
# (used by HTML libraries):

REGISTRY   = $(CURDIR)/Registry
SUBMITFORM = $(CURDIR)/SubmitForm

.PHONY: all compile install clean uninstall
.PHONY: cleanServers show showload sketch showall stop kill

all: install

install: compile

compile:
	$(MAKE) $(SUBMITFORM)
	$(MAKE) $(REGISTRY)

clean:
	$(CLEANCURRY)
	rm -f $(SUBMITFORM) $(REGISTRY)

uninstall: clean


$(SUBMITFORM): SubmitForm.curry HtmlCgi.curry \
	    $(LIBDIR)/NamedSocket.curry $(LIBDIR)/CPNS.curry
	$(REPL) --nocypm $(REPL_OPTS) :load SubmitForm :save :q

$(REGISTRY): Registry.curry HtmlCgi.curry
	$(REPL) --nocypm $(REPL_OPTS) :load Registry :save :q


cleanServers:
	$(REGISTRY) clean

show:
	$(REGISTRY) show

showload:
	$(REGISTRY) showload

sketch:
	$(REGISTRY) sketch

showall:
	$(REGISTRY) showall

stop:
	$(REGISTRY) stop

kill:
	$(REGISTRY) kill
