propertyfile
============

This package provides a library to read and update files
containing properties or configurations in the usual
equational syntax.

A property is defined by a line of the form

    prop=value

where `prop` starts with a letter.
All other lines (e.g., blank lines or lines starting with `#` are
considered as comment lines and are ignored.

