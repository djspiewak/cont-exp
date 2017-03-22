# `Cont` Experimentation

This repo is a bit of fluff to experiment with the `Cont` monad.  Specifically, I wanted to see if I could encode a useful `Task` solely in terms of `Cont` and `IO`.  The results are within.  Note that I ignore exceptions for the time being.

I don't believe this is a practical technique on the JVM, mostly due to the fact that `Cont` as formulated in Haskell cannot be written in a stack-safe fashion (though a modified `Cont` could be), which means that conventionally stack-safe constructors like `Task.delay`… uh, aren't stack safe.
