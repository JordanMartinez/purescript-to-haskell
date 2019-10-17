# Special Compiler Features

## Typed Holes

```purescript
foo :: String
foo = ?help
```

```haskell
foo :: String
foo = _

foo2 :: String
foo2 = _help    -- same thing
```

## Typed Wildcards

### Fail and Print Inferred Type

These will produce a compiler **error** and print the inferred type.
```purescript
foo :: ?Help
foo = "a string"
```

```haskell
foo :: _
foo = "a string"
```

### Warn and Use Inferred Type

These will produce compiler **warning** and print and use the inferred type

```purescript
bar :: _
bar = "a string"
```

```haskell
{-# LANGUAGE PartialTypeSignatures #-}

bar :: _
bar = "a string"

baz :: _someType -- also works
baz = "a string"
```
