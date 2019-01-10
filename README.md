pie-hs
======

A Haskell implementation of Pie, a small dependently-typed language.

What?
-----

Pie is a small dependently-typed programming language defined in _The Little Typer_ by Daniel P. Friedman and David Thrane Christiansen (MIT Press, 2018). Pie is similar to Scheme but includes dependent types as first-class elements of the language.

Consider this experimental research quality, not production quality. This is more for my own learning, but I hope it is useful to you if you're interested. I hope to include documentation, comments, and proper git branches/tags to make it easy to study and learn from, as well as simply give an example of a well-written Haskell project.

Also, consider this a work-in-progress, as many of the features described above do not exist yet.

Building and Running
--------------------

The Pie interpreter and library is itself written in Haskell. Aside from standard `ghc` compiler libraries, you will need `tasty` testing framework installed. To compile, you ideally will use `stack` since it can manage dependencies and the build.

Compile with

    stack build 

Then you can test the library with

    stack test
    
You can execute the Pie interpreter with

    stack exec pie
    
You can pass parameters to the actual executable and not `stack` by putting them after a `--`, such as `stack exec pie -- --help`.
    
References
----------

This implementation is based on the Pie language presented in the book:

* Friedman and Christiansen. _The Little Typer_. MIT Press, 2018.

However, the original implementation uses Racket. Therefore I consulted the following for help in implementing language interpretation using Haskell.

* Dornan, Chris, et al. _Alex User Guide_. <https://www.haskell.org/alex/doc/html/index.html>.
* Marlow, Simon, et al. _Happy User Guide_. <https://www.haskell.org/happy/doc/html/>.
* Bhattacharya, Jyotirmoy. _Alex and Happy: Lexers and Parsers in Haskell_. Lean Publishing, 2015. <https://leanpub.com/alexandhappy>.
* Aho, Alfred, et al. _Compilers: Principles, Techniques, and Tools_ (also known as "The Dragon Book"). 2nd Edition. Pearson Education, 2007.
* Ranta, Aarne. _Implementing Programming Languages: An Introduction to Compilers and Interpreters_. College Publications, 2012.
* Pfenning, Frank, et al. _CMU 15-411 Lecture Notes_. <https://www.cs.cmu.edu/~fp/courses/15411-f13/>.
* Diehl, Stephen. _Write You a Haskell_. <http://dev.stephendiehl.com/fun/>.

License
-------

This software and accompanying documentation are Copyright (C) 2018 Garret Wassermann.

This software distribution (including any documentation) is licensed under the GNU General Public License version 3 (GNU GPLv3). Please see the included COPYING file with this distribution.
