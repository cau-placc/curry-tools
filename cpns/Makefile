# Compile the CPNS (Curry Port Name Server) demon
# to support named ports in distributed Curry programs:

TOOL    = $(CURDIR)/CPNSD
LOGFILE = "/tmp/CurryPNSD.log"

# set additional options for Profile.getProcessInfos used in CPNS logging:
ifeq ($(CURRYSYSTEM),kics2)
ADD_OPTS = :set rts -T
endif

.PHONY: all compile install clean uninstall show showlog

all: install

compile: $(TOOL)

install: compile

clean:
	./stop
	$(CLEANCURRY)
	rm -f $(TOOL)

uninstall: clean


$(TOOL): $(LIBDIR)/CPNS.curry
	$(REPL) --nocypm $(REPL_OPTS) $(ADD_OPTS) :load CPNSD :save :q

# Show the currently registered ports:
show:
	$(TOOL) show

# Show the log file of the demon:
showlog:
	cat $(LOGFILE)

