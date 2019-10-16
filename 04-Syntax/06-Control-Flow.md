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

## `do` expressions

## `ado` expressions
