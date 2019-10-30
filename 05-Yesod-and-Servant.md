# Yesod and Servant

## Overview of Both

Servant
- uses template-haskell to reduce boilerplate for some things
- uses type-level programming to guarantee that routes are correct
- uses type-level programming to associate a type-level route with its corresponding handler
- uses type classes extensively to do practically everything, including serialization.

| Pros | Cons |
| - | - |
| - easy to understand and use | - restricts you to using type class codecs (e.g. `aeson`) rather than value-level codecs (e.g. `purescript-codec`) |
| - provides good documentation on routes out-of-box | |
| - handles the boring boilerplate for you | |

Philosophically, I disagree with Servant's design tradeoffs due to the type class based codecs due to reading [Some Thoughts on Typeclass-based Codecs](http://code.slipthrough.net/2018/03/13/thoughts-on-typeclass-codecs/)

Yesod (wip)
- uses template-haskell to reduce boilerplate for some things

## Real World Demos of Yesod and Servant

- [Real World - Yesod (server) + Persistent-created SQL](https://github.com/tzemanovic/haskell-yesod-realworld-example-app)
- [Real World - Servant (server) + hand-written SQL](https://github.com/dorlowd/haskell-servant-realworld-example-app)
- [Real World - Servant (server) + BEAM-created SQL](https://github.com/boxyoman/haskell-realworld-example)

## Performance and Benchmarks

Both frameworks use `warp` as their web server. [The Performance of Open Source Applications](https://www.aosabook.org/en/posa/warp.html) explains why warp is fast.

See how Yesod and Servant compare with other frameworks via the [TechEmpower Framework Benchmakrs](https://www.techempower.com/benchmarks/). As always, take these benchmarks with a grain of salt and read over their [methodology for producing these results](https://www.techempower.com/benchmarks/#section=motivation&hw=ph&test=fortune).

## Overview of Dependencies

### Both

- `warp` functions very similiarly to [`purescript-httpure`](https://www.aosabook.org/en/posa/warp.html)

### Yesod

TODO

### Servant

TODO
