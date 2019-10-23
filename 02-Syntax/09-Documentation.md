# Documentation

PureScript documentation gets converted to Markdown. It does not support links to other modules, functions, values, type classes, or their instances. (While this can be 'hacked in' by inserting a link that will "just so happen" to get rendered as a link to the corresponding entity, it's a hack.)

Haskell's documentation is a lot more powerful. See [Haddock User Guide](https://www.haskell.org/haddock/doc/html/index.html) for full explanation.

## Basic Concepts

```purescript
-- Only documentation version

-- | This is documentation
-- | and every line must include that vertical bar (i.e. '|')
-- |
-- | This is a new paragraph.
-- This is a comment, not documentation.
```

```haskell
-- First documentation version

-- | This is documentation
-- and every comment that follows is considered part of the documentation
-- Once a non-comment line is reached, the documentation stops.
--
-- This begins a new paragraph
-- This is documentation, not a comment.

-- Second documentation version
{-|A multi-line
  documentation
that is sometimes easier to write
-}

-- Third version

entity -- ^ Documentation for entity

entity2
  -- ^ Documentation for entity2
```

## Values

```purescript
-- | Documentation on a value
value :: Int
value = 4
```

```haskell
-- | Documentation on a value
value :: Int
value = 4

value2 :: Int
-- ^ This is also documentation.
value2 = 4

{-| This is a multi-line
documentation
-}
value3 :: Int
value3 = 4
```

## Functions

```purescript
-- | Documentation on a function
function :: Int -> String
function _ = "easy"
```

```haskell
-- | Documentation on a function2
function2
  :: Int -- ^ Doc on Int argument
  -> String -- ^ Doc on String argument
function2 _ = "easy"

function3
  :: forall a.
  a ->
  -- ^ Doc on `a` argument
  String
  -- ^ Doc on String argument
function3 _ = "easy"
```

## Data Types

```purescript
-- | Documentation on a given data type
data SomeData
  -- | Documentation on a particular data constructor
  = SomeData

-- | Documentation on a given type alias
type MyType = String

-- | Documentation on a given newtype
newtype SmallInt = SmallInt Int
```

```haskell
-- | Documentation on a given data type
data SomeData
  -- | Documentation on a particular data constructor
  = SomeData
  | OtherData -- ^ Also documentation for constructor

  -- | Documentation on a given type alias
  type MyType = String

  -- | Documentation on a given newtype
  newtype SmallInt = SmallInt Int
```

## Records


```purescript
-- | Documentation for a record
type Record = { foo :: String, bar :: Baz }
```

```haskell
-- | Documentation on a given data type
data Record =
  Record { -- | Documentation on `foo` field
           recordFoo :: String
         , recordBar :: Baz -- ^ Documentation on `bar` field
         }
  -- | Documentation on a given type alias
  type MyType = String

  -- | Documentation on a given newtype
  newtype SmallInt = SmallInt Int
```

## Type Classes Definitions

```purescript
-- | Documentation on a given type class
class MyClass a b | a -> b where
  -- | Documentation for a particular function/value defined in a type class
  myFunction :: a -> b
```

```haskell
-- | Documentation on a given type class
class MyClass a b | a -> b where
  -- | Documentation for a particular function/value defined in a type class
  myFunction :: a -> b

  otherValue :: a -- ^ This value's documentation
```

## Type Class Instances

```purescript
-- | Documentation for a particular instance of a type class
instance example :: MyClass String Int where
  myFunction _ = 4
```

```haskell
-- | Documentation for a particular instance of a type class
instance MyClass String Int where
  myFunction _ = 4
```

## Modules

```purescript
-- | Docs for module
module MyModule
  ( foo
  , bar
  , baz
  ) where
```

```haskell
-- | Docs for module
module MyModule
  ( foo -- * Header1
  , bar -- ** Header2
  , baz -- *** Header3

  , comesAfterBaz
  ) where
```
