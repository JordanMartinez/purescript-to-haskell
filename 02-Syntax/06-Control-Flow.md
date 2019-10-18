# Control Flow

## If Then Else Statements

### Basic

```purescript
foo :: String -> String
foo x =
  if x == "a" then "foo"
  else if x == "b" then "bar"
  else "baz"
```

```haskell
foo :: String -> String
foo x y =
  if x == "a" then "foo"
  else if x == "b" and y == "c" then "bar"
  else "baz"
```

### Cleaner Syntax

The same example above can be rewritten via this Haskell-only syntax.

```haskell
{-# LANGUAGE MultiWayIf #-}

foo2 :: String -> String -> string
foo2 x y =
  if | x == "a"              -> "foo"
     | x == "b" and y == "c" -> "bar"
     | otherwise             -> "baz"
```

## Applicative Do Notation

Applicative do works out-of-box with PureScript. Haskell requires the `ApplicativeDo` language extension.

```purescript
fooDesugared :: Maybe Int
fooDesugared = \a b _ -> a + b
  <$> comp1
  <*> comp2
  <*> comp3

fooSugared :: Maybe Int
fooSugared = ado
  a <- comp1
  b <- comp2
  comp3
  in a + b
```

```purescript
{-# LANGUAGE ApplicativeDo #-}

fooSugared :: Maybe Int
fooSugared = ado
  a <- comp1
  b <- comp2
  comp3
  in a + b
```

## Rebindable Do and Ado Notation

PureScript can rebind do and ado notation out-of-box using two methods. Haskell can rebind its syntax when the `RebindaleSyntax` extension is enabled. It can rebind `do` notation, but different functions have to be rebound than PureScript's corresponding version. I don't think Haskell can rebind `ado` notation. Lastly, Haskell can rebind more of its syntax than PureScript can.

```purescript
foo =
  -- local rebinding
  let
    bind = {- impl -}
    discard = {- impl -}
  in do
    a <- comp1
    comp2
    pure a

foo2 = MyModule.do   -- qualified rebinding
  a <- comp1
  comp2
  pure a
```

```haskell
foo =
  -- local rebinding
  let
    >>= = {- impl -}
    pure = {- impl -}
  in do
    a <- comp1
    comp2
    pure a
```
