# data-lens-light

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/UnkindPartition/data-lens-light/ci.yaml?branch=master)](https://github.com/UnkindPartition/data-lens-light/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/data-lens-light.svg?color=success)](https://hackage.haskell.org/package/data-lens-light)
[![Dependencies](https://img.shields.io/hackage-deps/v/data-lens-light?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=data-lens-light)

This is a minimalistic lens library, based on data-lens.

The main differences from data-lens are:

* reduced API
* minimal set of dependencies
* data-lens, data-lens-fd and data-lens-template exported through a single
  module in a single package
* `MonadState` combinators return `()`
* strict `MonadState` combinators force the whole state, too

(The above list may be not exhaustive. Compatibility with data-lens is not a
goal.)
