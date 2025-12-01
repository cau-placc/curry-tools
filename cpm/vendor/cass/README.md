# CASS: The Curry Analysis Server System

This directory contains the implementation of CASS,
a generic and distributed analysis system for Curry programs.

## Overview

CASS (Curry Analysis Server System) is a tool for the analysis of Curry 
programs. CASS is generic so that various kinds of analyses
(e.g., groundness, non-determinism, demanded arguments) can easily
be integrated into CASS.
Currently, CASS contains more than 30 kinds of program analyses
which are used by various programming tools for Curry.
The implementation of these analyses is contained in the package
[cass-analysis](https://cpm.curry-lang.org/pkgs/cass-analysis.html)

In order to analyze larger applications consisting of dozens or hundreds
of Curry modules, CASS supports a modular and incremental analysis of programs.
Moreover, it can be used by different programming tools,
like documentation generators, analysis environments, program optimizers,
as well as Eclipse-based development environments.
For this purpose, CASS can also be invoked as a server system
to get a language-independent access to its functionality.
CASS is completely implemented Curry as a master/worker architecture
to exploit parallel or distributed execution environments.

## Documentation

The structure of CASS and the ideas behind its implementation
can be found in this paper:

M. Hanus, F. Skrlac:
[A Modular and Generic Analysis Server System for Functional Logic Programs](https://dx.doi.org/10.1145/2543728.2543744),
ACM SIGPLAN 2014 Workshop on Partial Evaluation and Program Manipulation (PEPM'14), ACM Press, pp. 181-188, 2014


## Usage

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
are temporarily stored in the file `$HOME/.cass.port`
(in the tuple format `(port,pid)`).

The program `cass` can also be started on a console with arguments:

    cass <analysis> <module>

In this case, the analysis with the specified name is applied
to the specified module without the use of the server protocol
and the output is shown on stdout. Run the command

    cass --help

to get a description of the arguments and a list of registered analysis
names.

The analysis system can be configured in the file `$HOME/.cassrc`
which is installed after the first run of the system.

More details about the usage of CASS can be found in a
[short user manual](https://cpm.curry-lang.org/DOC/cass-4.0.0/main.pdf).


### Examples:

The Curry program `UsingCass` in directory `examples` contains
a simple program demonstrating the use of analysis results
computed by CASS inside another Curry program.

## Web Demo Installation

If you want to try CASS on simple programs via a web interface,
you can use a
[Web Demo Installation](https://cpm.curry-lang.org/webapps/cass/)
of CASS.


## Technical details

### Description of some Curry modules:

* `CASS.Registry`: All available analyses must be registered here.
* `CASS.Server`: The main module implementing the use of the server.
* `CASS.ServerFormats`: Definition and implementation of output formats.
* `CASS.WorkerFunctions`: Implementation of the analysis workers
  (in particular, alternative fixpoint iterations to compute
   dependency analyses, see option `fixpoint` in the configuration file,
   must be inserted here).

### Files

In order to support a modular and incremental analysis of applications
consisting of several modules, CASS caches already computed analysis results
of modules in under the directory `~/.curry_analysis_cache`.
This path is defined in `Analysis.Files`
(a module contained in the package `cass-analysis`).
To store the analysis results for different Curry systems and
abstract domains separately, the analysis results for a Curry module
which is stored in a file with path `MODULEPATH.curry` are contained in

    ~/.curry_analysis_cache/CURRYSYSTEMID/MODULEPATH.*

For instance, the results of the analysis `Overlapping` of the prelude
for KiCS2 Version 3.1.0

    ~/.curry_analysis_cache/kics2-3.1.0/opt/kics2/lib/Prelude.Overlapping.*

provided that KiCS2 is installed at `/opt/kics2`, i.e., the prelude
is contained in the file `/opt/kics2/lib/Prelude.curry`.
Actually, there are four files (ending with `pub`, `priv`, `pub.rw`, `priv.rw`)
for each analysis result (for the public and private operations in
plain term representation and a compact term representation).

All cache files (for the Curry system used to install this tool)
for an anlysis can be deleted by the command

    cass --delete <analysis>

This might be useful if the implementation of an analysis has been changed.

-----------------------------------------------------------------------

Contact: [Michael Hanus](https://www.michaelhanus.de/)
