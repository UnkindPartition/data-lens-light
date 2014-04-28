data-lens-light
===============

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
