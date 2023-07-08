keyl
====

A Haskell implementation of Pie, a small Scheme-like dependently-typed language.

# What?

Pie is a small dependently-typed programming language defined in _The Little Typer_ by Daniel P. Friedman and David Thrane Christiansen (MIT Press, 2018). Pie is similar to Scheme but includes dependent types as first-class elements of the language.

Consider this experimental research quality, not production quality. This is more for my own learning, but I hope it is useful to you if you're interested. I hope to include documentation, comments, and proper git branches/tags to make it easy to study and learn from, as well as simply give an example of a well-written modern Haskell project. It's not there yet but hey gotta have aspirations.

So to reiterate, consider this a work-in-progress.

## Why the Name?

`keyl` is short for "key lime", my favorite kind of Pie. Didn't seem to be a name used by any other projects in a quick internet search so became the name to differentiate this project from the Pie language which in theory could have other interpretations, etc.

# Building and Running

The `keyl` interpreter and library for Pie is itself written in Haskell. Aside from standard `ghc` compiler libraries, you will need `tasty` testing framework installed. To compile, you ideally will use `stack` since it can manage dependencies and the build.

Highly recommended that you install and use `ghcup` to manage your Haskell development tools/packages. See: [haskell.org](http://haskell.org) for more information.

Once you have the Haskell development tools installed:

Compile with

    stack build 

Then you can test the library with

    stack test
    
You can execute the Pie interpreter with

    stack exec keyl
    
You can pass parameters to the actual executable and not `stack` by putting them after a `--`, such as `stack exec keyl -- --help`.
    
# References

This implementation is based on the Pie language presented in the book:

* Friedman and Christiansen. _The Little Typer_. MIT Press, 2018.

Which references the following related books on Scheme:

* Friedman and Felleisen. _The Little Schemer_, 4th Edition. MIT Press, 1996.
* Friedman and Felleisen. _The Seasoned Schemer_. MIT Press, 1996.

However, the original implementation of Pie provided by the book authors uses Racket. The following were helpful directly or indirectly in better understanding how compilers work and in implementing language interpretation using Haskell.

* Wespiser, Adam. _Write You A Scheme, Version 2.0_. 2016. <https://wespiser.com/writings/wyas/00_overview.html>.
* Tang, Jonathan, with Wikibooks contributors. _Write Yourself a Scheme in 48 Hours_. Wikibooks, 2007. <https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours>.
* Diehl, Stephen. _Write You a Haskell_ (early draft). 2015. <http://dev.stephendiehl.com/fun/>.
* Dornan, Chris, et al. _Alex User Guide_. <https://www.haskell.org/alex/doc/html/index.html>.
* Marlow, Simon, et al. _Happy User Guide_. <https://www.haskell.org/happy/doc/html/>.
* Bhattacharya, Jyotirmoy. _Alex and Happy: Lexers and Parsers in Haskell_. Lean Publishing, 2015. <https://leanpub.com/alexandhappy>.
* Aho, Alfred, et al. _Compilers: Principles, Techniques, and Tools_ (also known as "The Dragon Book"). 2nd Edition. Pearson Education, 2007.
* Ranta, Aarne. _Implementing Programming Languages: An Introduction to Compilers and Interpreters_. College Publications, 2012.
* Pfenning, Frank, et al. _CMU 15-411 Lecture Notes_. <https://www.cs.cmu.edu/~fp/courses/15411-f13/>.

# License

This software and accompanying documentation are Copyright (C) 2023 Garret Wassermann.

This software distribution (including any documentation) is licensed under the GNU General Public License version 3 (GNU GPLv3). Please see the included COPYING file with this distribution.
