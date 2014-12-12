# Pretty : A Haskell Pretty-printer library

[![Hackage version](https://img.shields.io/hackage/v/pretty.svg?style=flat)](https://hackage.haskell.org/package/pretty) [![Build Status](https://img.shields.io/travis/haskell/pretty.svg?style=flat)](https://travis-ci.org/haskell/pretty)

Pretty is a pretty-printing library, a set of API's that provides a
way to easily print out text in a consistent format of your choosing.
This is useful for compilers and related tools.

It is based on the pretty-printer outlined in the  paper 'The Design
of a Pretty-printing Library' by John Hughes in Advanced Functional
Programming, 1995. It can be found
[here](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.8777).


## Licensing

This library is BSD-licensed.

## Building

The library uses the Cabal build system, so building is simply a
matter of running

```
cabal configure --enable-tests
cabal build
```

## Branches

Usually two branches are maintained for Pretty development:

 * master: This branch is generally kept in a stable state and is
   where release are pulled and made from. The reason for this is GHC
   includes the pretty library and tracks the master branch by default
   so we don't want experimental code being pulled into GHC at times.

 * next: This branch is the general development branch.

## Get involved!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/haskell/pretty/issues).

Master [git repository](http://github.com/haskell/pretty):

* `git clone git://github.com/haskell/pretty.git`

## Authors

This library is maintained by David Terei, <code@davidterei.com>. It
was originally designed by John Hughes's and since heavily modified by
Simon Peyton Jones.

