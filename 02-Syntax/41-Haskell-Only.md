# Haskell Only

This file covers things that only exist in Haskell and have no correspondent in PureScript.

## View Patterns

To make code less verbose, the [ViewPatterns](https://limperg.de/ghc-extensions/#viewpatterns) language extension allows you to pattern match using the result of a function applied to the argument.

If this type of pattern match will be used throughout your project via an import, you should also enable the [PatternSynonyms](https://limperg.de/ghc-extensions/#patternsynonyms) language extension, which makes view patterns a bit more readable.

## PolyKinds

In PureScript, we need to use one `Proxy` type for each type-level value (e.g. `SProxy`, `BProxy`, `OProxy`, `RProxy`, etc.). Haskell has [`PolyKinds`](https://limperg.de/ghc-extensions/#polykinds) (i.e. polymorphic kinds), which allows developers to use one `Proxy` type that can be used to store various type-level values. PureScript will likely get PolyKinds sometime in the future.

## GADTs

Read through the [Haskell wikibook on GADTs](https://en.wikibooks.org/wiki/Haskell/GADT#Summary) to understand how they work and what they enable. In short, they allow you to use phantom types to encode additional information at the type-level. As a result, some runtime errors can be checked at compile time.

Read through [`GADTSyntax`](https://limperg.de/ghc-extensions/#gadtsyntax) to understnd how the syntax works and then [`GADTs`](https://limperg.de/ghc-extensions/#gadts) to understand how they can be modified.

## Type Families

In PureScript, we use multi-parameter type classes and functional dependencies to do type-level programming in a way similar to the Prolog programming language. [`TypeFamilies`](https://limperg.de/ghc-extensions/#typefamilies) and [`TypeFamilyDependencies`](https://limperg.de/ghc-extensions/#typefamilydependencies) provide type-level programming but with a bit more readability than the Prolog way.

AFAIK, type families are just a different way to do type-level programming; I believe that what one can do with type families can also be done with multi-parameter type classes and functional dependencies.

## Recursive `do`

See the [`RecursiveDo`](https://limperg.de/ghc-extensions/#recursivedo) language extension.

## Template Haskell

This is a somewhat controversial extension. Many use it to reduce the time spent writing boilerplate correctly.

See the [`TemplateHaskell`](https://limperg.de/ghc-extensions/#templatehaskell) language extension.

## Postfix Symbolic Aliases

See the [`PostfixOperators`](https://limperg.de/ghc-extensions/#postfixoperators) language extension

## List Comprehensions

You may need to know the "list comprehension" syntax to read other's source code or to write one-liners. I don't think I would recommend using this syntax.

In PureScript, we might write the following:
```purescript
someValue :: List String
someValue = do
  elemFromList1 <- list1
  elemFromList2 <- list2
  pure (show (elemFromList1 + elemFromList2))
  -- where
  --   list1 = 1 : 2 : 3 : Nil
  --   list2 = 4 : 5 : 6 : Nil
```

We could write the same thing in Haskell using `do` notation. Or we could use "list comprehensions." The same code above can be rewritten to the following:
```haskell
someValue =
  [ show (elemFromList1 + elemFromList2) | elemFromList1 <- list1, elemFromList2 <- list2 ]
```
