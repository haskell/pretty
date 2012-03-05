Pretty : A Haskell Pretty-printer library
------------------------------------------------------------------------------
This repository contains a pretty-printing library, a set of API's that
provides a way to easily print out text in a consistent format of your
choosing. This is useful for compilers and related tools. The library was
originally designed by John Hughes's and has since been heavily modified by
Simon Peyton Jones.

It is based on the pretty-printer outlined in the  paper 'The Design of a
Pretty-printing Library' in Advanced Functional Programming, Johan Jeuring and
Erik Meijer (eds), LNCS 925 <http://www.cs.chalmers.se/~rjmh/Papers/pretty.ps>

The library uses the Cabal build system, so building is simply a matter of
running 'cabal install' or 'cabal configure && cabal build'.

Usually two branches are maintained for Pretty development:

 * master: This branch is generally kept in a stable state and is where
 release are pulled and made from. The reason for this is GHC includes
 the pretty library and tracks the master branch by default so we don't
 want experimental code being pulled into GHC at times.

 * next: This branch is the general development branch.

