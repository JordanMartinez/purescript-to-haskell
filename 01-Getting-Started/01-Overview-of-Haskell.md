# Overview of Haskell

## Haskell the Language

Haskell the language is defined by the Haskell standard. This standard is redefined every so often. The original Haskell specification was Haskell 1998. The latest one is [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/).

After a standard has been defined, anyone can propose adding a language extension to change how the language works. Since these divert from the standard, language extensions are completely opt-in: one has to enable them to use them.
Extensions typically change what the syntax does, enables new syntax, enables new type system features, or other things like that. Developers propose and use language extensions to experiment with ideas. If an experimentation is successsful and many people begin to use such an extension, it can become a part of the Haskell standard.

Since it has been ~9 years since the last standard was defined, there are a large number of language extensions that are used by most Haskell programmers but which aren't currently a part of the standard. When a new standard is defined, many of theses will likely become a part of the standard and will no longer need to be turned on. Unfortunately, the upcoming standard, Haskell 2020, seems to have been delayed due to "diffusion of responsibility" ([original comment](https://github.com/haskell/rfcs/issues/15#issuecomment-302138648) and [comment providing more context](https://github.com/haskell/rfcs/issues/15#issuecomment-302146265))

## Haskell's Developer Tools

These tools will be covered more in depth in the `Build Tools` folder.

### Compiler Tools

Since the standard defines what Haskell is, a compiler is what turns Haskell source code into a working program. The Glasgow Haskell Compiler (GHC) is the state-of-the-art Haskell compiler. While there are other Haskell compilers, most refer to GHC when they say they program in Haskell.

GHC comes with a tool called GHCi (i.e. GHC interactive). This is the GHC's REPL. It is vastly more powerful than PureScript's REPL.

### Dependency Managers / Build Tools

#### Haskell uses Package Version Policy (PVP), not Semantic Versioning (SemVer)

Semantic Versioning: MAJOR.MINOR.PATCH
Haskell's Package Version Policy: MAJOR.MAJOR.MINOR.PATCH

See [Haskell PVP FAQ](https://pvp.haskell.org/faq/) for a good explanation.

#### Use `hpack` to generate `.cabal` files via `package.yml` files

`.cabal` files store metadata about a package (e.g. author, contact info, dependencies, language extensions enabled by default, etc.). This is the file format used in the upcoming build tools. Unfortunately, the file format used by `.cabal` is verbose.

[`hpack`](https://github.com/sol/hpack#readme) is a tool that can read and use `package.yml` files to produce valid `.cabal` files. `package.yml` files come with three advantages over `.cabal` files. They...
> Don't require the user to state the obvious; make sensible assumptions by default
> Give the user 100% control when needed
> Don't require the user to repeat things; facilitate DRY-ness

As we'll see in the "Syntax" folder, there are a lot of language extensions you will likely want to enable. Moreover, there are GHC options you will likely want to turn on by default. Rather than enabling each extension in each file or including each GHC option in the CLI, it can be easier to add these across the entire project via your `package.yml` file.

Both `cabal` (lowercased 'c') and `stack` utilize the `.cabal` file. So, it's often better to edit your `package.yml` file, use `hpack` to generate a valid `.cabal` file, and let the tools use that generated `.cabal` file.

#### Cabal and Stack

The community is strongly split between `Cabal`/`cabal` and `stack`. Originally, there was only `Cabal`/`cabal`. `Stack` came about to fix a "dependency hell" problem with Cabal that arose due to transitive dependencies. `Cabal`/`cabal` then fixed this problem and added a few other features. I think they have similar features now.

`cabal` has "v1" commands and "v2" commands. You should always use the "v2" commands.

PureScript developers tend to use one of two tools to install dependencies and build their code:
1. `bower` + `pulp`
2. `package-sets` + `spago` (the dependency manager parts) and `spago` (the build tool part).

The `bower + pulp` workflow has similarities to Haskell's `Cabal` library (i.e. the library that reads `.cabal` files and builds projects) and `cabal` tool (i.e. the tool that people use to package Haskell projects).
Similarly, the package sets used by PureScript's `spago` for dependency management and the build tool aspect of `spago` is similar to the Haskell's `Stack`. `Stack` uses a curated set of packages that is defined via resolvers (resolvers are similar to [`purescript/package-sets` releases](https://github.com/purescript/package-sets/releases)). Unlike `package-sets`, `stack` does not use Dhall to produce the curated sets.

For a somewhat longer overview of `cabal` and `stack`, see [Why not both?](https://medium.com/@fommil/why-not-both-8adadb71a5ed)

## Documentation Tools

| Name | PureScript Corresponding Tool | Clarifications |
| - | - | - |
| Hackage | Pursuit without a search engine | Only provides the documentation. Nothing else |
| Hoogle | Pursuit's search engine | Via a search, one can find the corresponding Hackage documentation for an entity (i.e. package, module, function, value, type class, etc.)
| Stackage | -- | Stackage is what we would get if Spago created an instance of Pursuit that only had documentation for packages in a given package set.

## Miscellaneous Tools

- [`hlint`](https://github.com/ndmitchell/hlint) is a tool that provides suggestions for improving your code.
