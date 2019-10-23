# purescript-to-haskell

## Purpose of this Repository

[Via the Feynman Technique](https://medium.com/taking-note/learning-from-the-feynman-technique-5373014ad230), this repository exists to help me understand how to use Haskell on the backend via Yesod or Servant with the goal of using PureScript on the front-end, and to help anyone else who is walking along a similar path.

See the [Table of Contents file](../table-of-contents.md).

### Goals of this library

- [Done] - Explain how to install Haskell and setup a developer environment
- [Done] - Overview Haskell's REPL
- [Done] - Show the similarities, differences, and gotchas between PureScript and Haskell Syntax
- [Done] - Overview which language extensions one should enable to get a Haskell developer experience that is near enough to a PureScript experience and how that affects the syntax
- [Done] - Cover the main differences between Stack and Cabal, the two main build tools for Haskell
- Cover other gotchas PS developers may experience
- Overview the dependencies used in Yesod and Servant (e.g. aeson, text, blaze, etc.)
- Overview Yesod
    - Start with [Yesod for Haskellers](https://www.yesodweb.com/book/yesod-for-haskellers) to learn how it works without Template Haskell
    - Then read the rest of the book to understand how Template Haskell reduces the boilerplate one would normally write
- Overview Servant
    - Don't use the [stable branch's tutorial](https://haskell-servant.readthedocs.io/en/stable/tutorial/)
    - Use the [master branch's tutorial](https://docs.servant.dev/en/master/tutorial/index.html)
- Provide sample `.package.yml` file for Yesod and Servant

### Non-Goals of this library

- Teach one how to use Haskell or the functional paradigm
- Teach PureScript to a Haskell programmer
- Rewrite what has already been written

## How I got Here

I spent the previous year [learning PureScript using the Feynman Technique](https://github.com/jordanmartinez/purescript-jordans-reference), so that I could build websites and applications that
1. are much safer than what JavaScript and TypeScript can offer
2. can utilize more powerful language features and abstractions than what Elm can offer
3. and are easier to setup than GHCJS or alternatives

While I could build a front-end using libraries like [purescript-halogen](https://github.com/slamdata/purescript-halogen)/[purescript-ocelot](https://github.com/citizennet/purescript-ocelot) or [purescript-react-basic](https://github.com/lumihq/purescript-react-basic), building a web server that interacts with such a front-end is a different matter.

PureScript can be used to produce a Node.js web server. However, the two main contenders, [purescript-httpure](https://github.com/cprussin/purescript-httpure) and [hyper](https://github.com/purescript-hyper/hyper), are still immature in a few areas. Moreover, both of their contributors are busy focused on other things. So, it seems unlikely that these libraries will provide the "out-of-box ready" environment I want.

However, JavaScript is not the only language to which PureScript can compile. It can also compile to languages such as C++, Go, and Kotlin (a recent backend that is still being developed). Unfortunately, none of these PureScript backends have a mature ecosystem for building a webserver. Moreover, supporting non-JavaScript backends in the PureScript compiler is still something that needs work. I believe this situation will improve in the next few years, but I unfortunately can't wait that long.

After asking on the `#purescript` FP Slack channel, I was encouraged to consider using Haskell to build a web server using either the Yesod or Servant frameworks. Since Haskell is similar to PureScript in a number of ways, I believed this approach would achieve my goal in an easier manner than attempting to build a mature web server in PureScript via one of its backends.
