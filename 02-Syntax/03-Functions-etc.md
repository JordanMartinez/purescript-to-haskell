# Functions etc.

## Functions

### Basic

The languages work the same for defining and using functions

```purescript
function :: Int -> Int
function 4 = 4

lambda :: Int -> Int
lambda = \x -> x + 1

higherOrderFunction1 :: Int -> (Int -> Int) -> (Int -> Int)
higherOrderFunction1 x f = \y -> (f y) + x

typeClassConstrainedFunction :: forall f. Foldable f => f Int -> Int
typeClassConstrainedFunction = foldl (+) 0
```

```haskell
function :: Int -> Int
function 4 = 4

lambda :: Int -> Int
lambda = \x -> x + 1

higherOrderFunction1 :: Int -> (Int -> Int) -> (Int -> Int)
higherOrderFunction1 x f = \y -> (f y) + x

typeClassConstrainedFunction :: Foldable f => f Int -> Int
typeClassConstrainedFunction = foldl (+) 0
```

## Let / In / Where Statements

```purescript
whereUsage :: Int -> Int
whereUsage x = x + y
  where y = 4

letUsage :: Int -> Int
letUsage x = let y = 4 in x + y

letWhereUsage :: Int -> Int
letWhereUsage x =
  let y = 4
  in x + y + z
  where z = 10
```

```haskell
whereUsage :: Int -> Int
whereUsage x = x + y
  where y = 4

letUsage :: Int -> Int
letUsage x = let y = 4 in x + y

letWhereUsage :: Int -> Int
letWhereUsage x =
  let y = 4
  in x + y + z
  where z = 10
```

## Abbreviated Function Bodies

These are the same in both languages.

```purescript
abbreviatedFunction2 :: Int -> String
abbreviatedFunction2   = show   {- is the same as...
abbreviatedFunction2 x = show x -}
```

```haskell
abbreviatedFunction2 :: Int -> String
abbreviatedFunction2   = show   {- is the same as...
abbreviatedFunction2 x = show x -}
```

## Pattern Matching

### Basic

```purescript
-- Given a data type like this:
data Alphabet
  = A
  | B
  | C

mkString :: Alphabet -> String
mkString A = "A"
mkString B = "B"
mkString C = "C"
```

```haskell
-- Given a data type like this:
data Alphabet
  = A
  | B
  | C

mkString :: Alphabet -> String
mkString A = "A"
mkString B = "B"
mkString C = "C"
```

### Matching Specific Values and a Catch-All Binding

```purescript
literalValue :: Int -> String
literalValue 0 = "0"
literalValue 1 = "1"
literalValue 2 = "2"
literalValue _   = "catch-all"
```

```haskell
literalValue :: Int -> String
literalValue 0 = "0"
literalValue 1 = "1"
literalValue 2 = "2"
literalValue _   = "catch-all"
```

### List and Array Pattern Matching

PureScript's `[]` pattern matching syntax works only on `Array`s. Haskell's works on `List`s, but can be made to work on any `Sequence`-like data structure if one enables the `OverloadedLists` language extension. Moreover, PureScript does not allow the usage of `1 : Nil` in the pattern matching of a list whereas Haskell does.

```purescript
array :: Array Int -> String
array []           = "an empty array"
array [0]          = "an array with one value that is 0"
array [0, 1]       = "an array with two values, 0 and 1"
array [0, 1, a, b] = "an array with four values, starting with 0 and 1 \
                       \ and binding the third and fouth to names 'a' and 'b'"
array [-1, _ ]     = "an array of two values, '-1' and another value that \
                       \ will not be used in the body of this function."
array _            = "catchall for arrays."

list :: List Int -> String
list Nil = "empty list"
list (Cons 1 Nil) = "List with one value that is 1"                       {-
list (1 : Nil) = "this would not compile"                                 -}
list (Cons x (Cons y tail)) = "another example"
```

```haskell
{-# LANGUAGE OverloadedLists #-}

import Data.Vector

array :: Vector Int -> String
array []           = "an empty array"
array [0]          = "an array with one value that is 0"
array [0, 1]       = "an array with two values, 0 and 1"
array [0, 1, a, b] = "an array with four values, starting with 0 and 1 \
                       \ and binding the third and fouth to names 'a' and 'b'"
array [-1, _ ]     = "an array of two values, '-1' and another value that \
                       \ will not be used in the body of this function."
array _            = "catchall for arrays."

list :: List Int -> String
list [] = "empty list"
list 1 : [] = "List with one value that is 1"
list x : y : tail = "another example"
```

### Unwrapping Data Constructors

The languages work the same.

```purescript
data A_Type
  = AnInt Int
  | Outer A_Type -- recursive type!
  | Inner Int

f :: A_Type -> String
f (Inner 0)             = "a value of type Inner whose value is 0"
f (Inner int)           = "a value of type Inner, binding its value to 'int' \
                          \name for usage in function body"
f (Outer (Inner int))   = "a value of type Outer, whose Inner value is bound \
                          \to `int` name for usage in function body"
f object@(AnInt 4)      = "a value of type AnInt whose value is '4', \
                          \binding the entire object to the `object` name for \
                          \usage in function body"
f _                     = "ignores input and matches everything; \
                          \acts as a default / catch all case"
```

```haskell
data A_Type
  = AnInt Int
  | Outer A_Type -- recursive type!
  | Inner Int

f :: A_Type -> String
f (Inner 0)             = "a value of type Inner whose value is 0"
f (Inner int)           = "a value of type Inner, binding its value to 'int' \
                          \name for usage in function body"
f (Outer (Inner int))   = "a value of type Outer, whose Inner value is bound \
                          \to `int` name for usage in function body"
f object@(AnInt 4)      = "a value of type AnInt whose value is '4', \
                          \binding the entire object to the `object` name for \
                          \usage in function body"
f _                     = "ignores input and matches everything; \
                          \acts as a default / catch all case"
```

### Regular Guards and Pattern Guards

PureScript's can use regular guards and pattern guards. Haskell can use regular guards, but pattern guards must be enabled via a language extension called `PatternGuards`.

```purescript
q :: Int -> Int -> String
q x y | x == 3                   = "simple guard"
      | x == 5, y == 5           = "multiple guards"
      | (Just 2) <- tryStuff x   = "pattern guard"

      | (Just 2) <- tryStuff x
      , y == 4                   = "pattern guard and regular guard"

      | (Just a) <- tryStuff x
      , (Just b) <- tryStuff y   = "last example"

      | otherwise                = "catch-all"
```

```haskell
{-# LANGUAGE PatternGuards #-}

q :: Int -> Int -> String
q x y | x == 3                   = "simple guard"
      | x == 5, y == 5           = "multiple guards"
      | (Just 2) <- tryStuff x   = "pattern guard" -- extension makes this work

      | (Just 2) <- tryStuff x
      , y == 4                   = "pattern guard and regular guard"

      | (Just a) <- tryStuff x
      , (Just b) <- tryStuff y   = "last example"

      | otherwise                = "catch-all"
```

## Usage of `forall`

### Basic

PureScript requires an explicit `forall` when using polymorphic types. Haskell does not.

```purescript
identity :: forall a. a -> a
```

```haskell
id :: a -> a
```

### The Meaning of the Same Type in Multiple Contexts

```purescript
doFoo :: forall a. Monoid a => a -> a
doFoo value = value <> anotherValue
  where
  anotherValue :: a -- this `a` is the SAME as the `a` above
  anotherValue = mempty
```

```haskell
-- notice that an explicit `forall` is missing in this example
doFoo :: Monoid a => a -> a
doFoo value = value <> anotherValue
  where
  anotherValue :: a -- this `a` is DIFFERENT than the `a` above
  anotherValue = mempty
```

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

-- notice that an explicit `forall` is required in this example
doFoo :: forall a. Monoid a => a -> a
doFoo value = value <> anotherValue
  where
  anotherValue :: a -- this `a` is the SAME as the `a` above
  anotherValue = mempty
```

### Nested Foralls (i.e. Rank-N Types)

Rank-N Types work out-of-box with PureScript. Haskell requires a language extension: [`RankNTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RankNTypes).

```purescript
foo :: forall a. (forall b. b -> a) -> a
```

```haskell
{-# LANGUAGE RankNTypes #-}

foo :: forall a. (forall b. b -> a) -> a
```


### Using Kind Signatures

PureScript's kind signatures work out-of-box. Haskell requires a language extension called `KindSignatures`.

```purescript
foo :: forall (someKind :: SomeKind) b. Proxy someKind -> b
foo = -- ...
```

```haskell
{-# LANGUAGE KindSignatures #-}

foo :: forall (someKind :: SomeKind) b. Proxy someKind -> b
foo = -- ...
```

## Case Statements

### Single Variable

```purescript
foo :: String -> String
foo x = case x of
  "some value" -> "first match"
  _ -> "catch all"
```

```haskell
foo :: String -> String
foo x = case x of
  "some value" -> "first match"
  _ -> "catch all"
```

### Multi Variable

```purescript
foo :: String -> String -> String
foo x = case x, y of
  "some value", "another value" -> "first match"
  _ -> "catch all"
```

```haskell
-- TODO: This fails to compile. Not sure whether this can be fixed or not...
foo :: String -> String -> String
foo x = case x, y of
  "some value", "another value" -> "first match"
  _ -> "catch all"
```

### Syntax Sugar

PureScript can use `case _ of` syntax to omit the name of the binding. Haskell requires the language extension `LambdaCase` and modifies its syntax slightly.

```purescript
foo :: String -> String
foo = case _ of
  "some value" -> "first match"
  _ -> "catch all"
```

```haskell
{-# LANGUAGE LambdaCase #-}

foo :: String -> String
foo = \case
  "some value" -> "first match"
  _ -> "catch all"
```

## Passing Constructs as Arguments to Functions

Constructs that are passed to functions as arguments work out-of-box with PureScript. Haskell requires the `BlockArguments` language extensions.
```purescript
foo someFunction useArgument = someFunction do
  a <- comp1
  b <- comp2 \argument -> useArgument argument
  comp3 case b of
    Foo a -> a
  pure a
```

```haskell
foo someFunction useArgument = someFunction $ do -- notice the $ here
  a <- comp1
  b <- comp2 (\argument -> useArgument argument) -- and parenthesis here
  comp3 $ case b of -- and $ again here
    Foo a -> a
  pure a
```

```haskell
{-# LANGUAGE BlockArguments #-}
foo someFunction = someFunction do -- no $ here
  a <- comp1
  comp2 \argument -> useArgument argument -- no parenthesis here
  comp3 case b of   -- no $ here
    Foo a -> a
  pure a
```

## Annotating a polymorphic type with a more specific type signature

PureScript does not have syntax for type application whereas Haskell does via the extension, `TypeApplications`.

```purescript
class Foldable f where
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> f b

foo :: Int
foo =
  let intArrayFold = foldl :: (Int -> Int -> Int) -> Int -> Array Int -> Int
  in intArrayFold (+) 0 [1, 2, 3]
```

```haskell
-- extension not enabled
foo :: Int
foo =
  let intArrayFold = foldl :: (Int -> Int -> Int) -> Int -> Array Int -> Int
  in intArrayFold (+) 0 [1, 2, 3]
```

```haskell
{-# LANGUAGE TypeApplications #-}
foo :: Int
foo =
  let intArrayFold = foldl @Vector @Int @Int
  in intArrayFold (+) 0 [1, 2, 3]
