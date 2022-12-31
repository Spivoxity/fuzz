# FUZZ 2000 #

This version of the fuzz type-checker for Z is released under an
MIT-style licence.  It's a later version than the commercial
ones, and includes a couple of new features, including automatic
re-ordering of paragraphs to respect def-before-use.  So it's better
than money can buy!

The Z language accepted is still that of the Z Reference Manual,
second edition.  There are no plans to support the Z standard.

## Documentation ##

Documentation can be found in the 'doc' subdirectory of the
repository.  Generating the documents from the LaTeX input requires
LaTeX 2.09, so I've included PDF files for convenience.

* Fuzz manual: [fuzzman-pub.pdf](https://github.com/Spivoxity/fuzz/blob/master/doc/fuzzman-pub.pdf), and a 2-up version for printing on A4: [fuzzman-2up-pub.pdf](https://github.com/Spivoxity/fuzz/blob/master/doc/fuzzman-2up-pub.pdf).
* A Z reference card: [refcard-pub.pdf](https://github.com/Spivoxity/fuzz/blob/master/doc/refcard-pub.pdf).
* A slightly different reference card that fits on one double-sided sheet of A4: [refcard3-pub.pdf](https://github.com/Spivoxity/fuzz/blob/master/doc/refcard3-pub.pdf).

## New Features ##

There are a couple of features of the type-checker that were not
in the last commercial version of fuzz:
* Use before definition (`-d` flag).  The type-checker can build a dependency graph of the specification before type-checking and topologically sort it. The upshot is that, with very few restrictions, you can put the paragraphs of a specification in whatever order best suits exposition.
* Lisp-style echoing (`-l` flag).  The type-checker will optionally echo each input paragraph in dependency order using a lisp-style syntax.  I've used this as a way of inputting specs into experimental analysis programs, saving the experimenter the trouble of parsing and doing dependency analysis.  It's trivial to write a parser for the lisp-style syntax in most languages: we've used OCaml in our experiments.

## Installation ##

The source of the type-checker is in the `src` directory, and builds
without problem on many common platforms.  To build and install on Linux:

1. The package comes with a 'configure' script created with GNU Autoconf: say `./configure` to run it (or `autoconf` to regenerate it from `configure.ac`.
2. Say `make` at the top level (or in the src subdirectory) to build the type-checker.
3. (Optional) Say `make test` to run some regression tests.  No output is expected from the `diff` comparisons.
4. Say `sudo make install` to install all the bits and pieces.
5. If your TeX implementation needs it, run `texhash` to update TeX's directory information.

That's all!

## Software Licence ##

Manual and software copyright &copy; 1988&ndash;20007 J. M. Spivey.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

<b>This software is provided by the author "as is" and any express or
implied warranties, including, but not limited to, the implied warranties
of merchantability and fitness for a particular purpose are disclaimed.
In no event shall the author be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not limited to,
procurement of substitute goods or services; loss of use, data, or profits;
or business interruption) however caused and on any theory of liability,
whether in contract, strict liability, or tort (including negligence or
otherwise) arising in any way out of the use of this software, even if
advised of the possibility of such damage.</b>
