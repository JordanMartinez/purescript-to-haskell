# Miscellaneous Keywords

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

### Case Statements

### Let / In / Where Statements
