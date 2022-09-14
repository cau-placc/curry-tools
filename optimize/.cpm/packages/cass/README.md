CASS: The Curry Analysis Server System
======================================

This directory contains the implementation of CASS,
a generic and distributed analysis system for Curry programs.

The analysis system is structured as a worker/server application
where the workers are triggered by the main server to analyse
individual modules.

The analysis system can also be used as a client from other
application programs by a socket interface.
The protocol of this interface is described in `Protocol.txt`.
The server is explicitly started by the program `cass`
(generated via `make`) or implicitly by application programs
that use of the operation `Configuration.getServerPortNumber`
to find out the port number to connect to the analysis server.
The port number can either be explicitly specified the starting
the main server program via

    cass -p <port>

or a free port number is chosen when the analysis server is started.
The current port and process numbers of a running analysis server
are temporarily stored in the file `$HOME/.curryanalysis.port`
(in the tuple format `(port,pid)`).

The program `cass` can also be started on a console with arguments:

    cass <analysis> <module>

In this case, the analysis with the specified name is applied
to the specified module without the use of the server protocol
and the output is shown on stdout. Run the command

    cass --help

to get a description of the arguments and a list of registered analysis
names.

The analysis system can be configured in the file `$HOME/.curryanalysisrc`
which is installed after the first run of the system.
The implementations of the individual analysis are
usually defined in the package `cass-analysis`).


Description of some Curry modules:
----------------------------------

* `CASS.Registry`: All available analyses must be registered here.
* `CASS.Server`: The main module implementing the use of the server.
* `CASS.ServerFormats`: Definition and implementation of output formats.
* `CASS.WorkerFunctions`: Implementation of the analysis workers
  (in particular, alternative fixpoint iterations to compute
   dependency analyses, see option `fixpoint` in the configuration file,
   must be inserted here).

Examples:
---------

The Curry program `UsingCass` in directory `examples` contains
a simple program demonstrating the use of analysis results
computed by CASS inside another Curry program.

-----------------------------------------------------------------------

Contact: [Michael Hanus](https://www.informatik.uni-kiel.de/~mh)
