transbooleq: A tool to transform Boolean equalities into constraints
====================================================================

This package contains the implementation of a transformation tool
that replaces Boolean equalities by equational constraints
in FlatCurry programs.

The tool is integrated into the compilation chain of PAKCS/KiCS2.
The motivation and ideas of this tool are described in the following papers:

Antoy, S., Hanus, M.: From Boolean Equalities to Constraints
Proceedings of the 25th International Symposium on Logic-based Program
Synthesis and Transformation (LOPSTR 2015), Springer LNCS 9527, 2015, pp. 73-88
http://dx.doi.org/10.1007/978-3-319-27436-2_5

Antoy, S., Hanus, M.: Transforming Boolean equalities into constraints
Formal Aspects of Computing, 29(3), 2017, pp. 475-494
http://dx.doi.org/10.1007/s00165-016-0399-6


Statistics about the number of transformations are shown
with increased verbosity levels. For instance, if one sets the
option "v2" in PAKCS/KiCS2, a summary of the number of transformation
is shown, with option "v3" more details (analysis infos, timings,
and functions where transformations are applied) are shown and
a CSV file with this information is generated.
