# Primitive Data Types

PureScript defines a few types in the compiler. These are automatically imported via the `Prim` module. Haskell has a package called `base` that includes a number of modules that operate similarly to `Prim`. These are normally imported via the `Prelude` module, which is imported implicitly. We'll talk about imports later.

## Comments

These are the same for both languages. Documentation is different and will be covered later:

```purescript
-- single-line comment
{-
multi-
line
comment
-}
```

```haskell
-- single-line comment
{-
multi-
line
comment
-}
```

## Data, Type, and Newtype

These are the same.

```puresript
-- Sum and product types
data TypeConstructor aType bType hktBy1 phantomType
  = NoArgs
  | Args Type1 Type2 Type3
  | FunctionArg (Type1 -> Type2)
  | NestedArg (Box Int)
  | DoubleNestedArg (Box (Box Int))
  | HigherKindedGenericType1 (hktBy1 Int)
  | HigherKindedGenericType2 (hktBy1 aType)
  | Recursive (TypeConstructor aType bType hktBy1 phantomType)
  | ArgMix Type_ (A -> B) bType (TypeConstructor aType bType hktBy1 phantomType)

type TypeAlias = OriginalType

newtype Age = Age OriginalType
```

```haskell
-- Sum and product types
data TypeConstructor aType bType hktBy1 phantomType
  = NoArgs
  | Args Type1 Type2 Type3
  | FunctionArg (Type1 -> Type2)
  | NestedArg (Box Int)
  | DoubleNestedArg (Box (Box Int))
  | HigherKindedGenericType1 (hktBy1 Int)
  | HigherKindedGenericType2 (hktBy1 aType)
  | Recursive (TypeConstructor aType bType hktBy1 phantomType)
  | ArgMix Type_ (A -> B) bType (TypeConstructor aType bType hktBy1 phantomType)

type TypeAlias = OriginalType

newtype Age = Age OriginalType
```

## Empty Data Types

PureScript can define data types that don't have a constructor (usually for phantom types that tag something at the type-level). Haskell requires the `EmptyDataDecl` language extension to be enabled.

```purescript
data PhantomType_NoConstructors
```

```haskell
{-# EmptyDataDecl #-}

data PhantomType_NoConstructors
```

## Primitive-ish Types

### Boolean

```purescript
trueValue :: Boolean
trueValue = true

falseValue :: Boolean
falseValue = false
```

```haskell
trueValue :: Bool
trueValue = True

falseValue :: Bool
falseValue = False
```

### Array and List

The syntax for PureScript's `Array` type is the syntax Haskell uses for `List`

```purescript
data List a
  = Nil
  | Cons a (List a)

intList :: List Int
intList = Cons 1 (Cons 2 (Cons 3 Nil))

infixr 4 Cons as :

anotherList :: List Int
anotherList = 1 : 2 : 3 : Nil

intArray :: Array Int
intArray = [1, 2, 3]
```

```haskell
-- type List a = [a]

intList :: [Int] -- i.e. `List Int`
intList = [1, 2, 3]

-- Separate file
import Data.Vector (generate) -- i.e. `forall a. Int -> (Int -> a) -> Vector a`

intArray :: Vector Int
intArray = generate 4 id -- i.e. `id :: forall a. a -> a`
```

#### The `OverloadedLists` Language Extension

```haskell
-- TODO
```

### Char

PureScript's `Char` is a single character (i.e. UFT-16 code **unit**) and cannot represent code point values greater than `0xFFFF`. Haskell's `Char` is "an enumeration whose values represent Unicode (or equivalently ISO/IEC 10646) code **point**s."


```purescript
aChar :: Char
aChar = 'x'
```

```haskell
aChar :: Char
aChar = 'x'
```

### String

#### The Type

PureScript's Strings are UTF-16 due to JavaScript. Haskell's `String`s are unfortunately an alias to `[Char]` (i.e. `List Char`). Moreover, its syntax `"example string"` is of type `String`/`[Char]`.

```purescript
stringValue :: String
stringValue = "I am a string"
```

```haskell
type String = [Char] -- aka `List Char`

notStringValue :: String -- aka `List Char`
notStringValue = "I am a singly-linked list of characters, not a String"
```

#### The Real Haskell String Types

Due to the above situation, everyone uses `Text` as the main `String` type instead of `[Char]`/`String` or `ByteString` when referring to a binary representation of a `Text` value.

Since the literal syntax `"string value"` will define a value that has type `[Char`], how can people easily create `Text` or `ByteString` values? They use one of two options:
- Use the corresponding type's `pack`/`unpack` function
- Enable the `OverloadedStrings` language extension.

```haskell
import Data.Text (pack)

realStringValue :: Text
realStringValue =
  pack "The output of `pack` will be the String data type you're used to."
```

```haskell
import Data.ByteString (pack)

binaryStringValue :: ByteString
binaryStringValue =
  -- i.e. "a string value"
  pack [97,32,115,116,114,105,110,103,32,118,97,108,117,101]
```

#### The `OverloadedStrings` Language Extension

```haskell
-- TODO
```


#### Syntax Sugar

```purescript
slashy_string_syntax :: String
slashy_string_syntax =
  "Enables multi-line strings that \
  \use slashes \
            \regardless of indentation \

    \and regardless of vertical space between them \

    \(though you can't put comments in that blank vertical space)"
    {-
    "This will fail \
    -- oh look a comment that breaks this!
    \to compile."
    -}

triple_quote_string_syntax :: String
triple_quote_string_syntax = """
  Multi-line string syntax that also ignores escaped characters, such as
  * . $ []
  It's useful for regular expressions
  """
```

```haskell
slashy_string_syntax :: String
slashy_string_syntax =
  "Enables multi-line strings that \
  \use slashes \
            \regardless of indentation \

    \and regardless of vertical space between them \

    \(though you can't put comments in that blank vertical space)"
    {-
    "This will fail \
    -- oh look a comment that breaks this!
    \to compile."
    -}

-- This doesn't compile. I'm not sure whether this can be defined in a
-- different way
-- triple_quote_string_syntax :: String
-- triple_quote_string_syntax = """
--   Multi-line string syntax that also ignores escaped characters, such as
--   * . $ []
--   It's useful for regular expressions
--   """
```

### Int

```purescript
anInt :: Int
anInt = -4 + 8
```

Haskell's Integers

Fixed Size

| # of Bits | Unsigned | Signed |
| - | - | - |
| 4 | Word | Int |
| 8 | Word8 | Int8 |
| 16 | Word16 | Int16 |
| 32 | Word32 | Int32 |
| 64 | Word64 | Int64 |

Arbitrarily long: `Integer`

```haskell
anInt :: Int    -- fixed size; interger overflow can occur
anInt = -4 + 8

aReallyBigInt :: Integer -- however big you want it
aReallyBigInt = 99999999999999999999999999999999999999999999999999999999999999
```

### Number

PureScript's `Number` is a double-precision floating-point number due to JavaScript. Haskell's corresponding type is `Double`.

```purescript
aNumber :: Number
aNumber = 4.0
```

```haskell
aNumber1 :: Float   -- single-precision floating-point numbers
aNumber1 = 4.0

aNumber2 :: Double  -- double-precision floating-point numbers
aNumber2 = 4.0
```

#### Syntax Sugar for Ints and Numbers

Since its `0.13.0` release, PureScript allows the usage of underscores to make numeric values more readable. Haskell allows this, too, but only if the `NumericLiterals` language extension is enabled.

```purescript
readableInt :: Int
readableInt = 1_000_000

readableNumber :: Number
readableNumber = 1_000_000.0
```

```haskell
{-# LANGUAGE NumericLiterals #-}
readableInt :: Int
readableInt = 1_000_000

readableNumber :: Double
readableNumber = 1_000_000.0
```

### Unit and Row Kinds

PureScript uses the `Unit` type and `unit` for its value; it uses `()` to denote an empty row and `(a :: String)` for a single closed row of types.

Haskell uses `()` to refer to both the `Unit` type and `unit` value.

```purescript
type EmptyRow = ()
type SingleRow = (a :: String)

data Unit = Unit

unit :: Unit
unit = Unit

provideAUnitValue :: Unit
provideAUnitValue = unit
```

```haskell
-- data () = () -- basic idea...

provideUnitValue :: ()
provideUnitValue = ()
```

### Unicode Syntax

PureScript supports the unicode alternatives to ASCII character sequences. Haskell requires the [`UnicodeSyntax` language extension](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#extension-UnicodeSyntax) to be enabled for this support to be added.

```purescript
-- Not all Unicode alternatives are shown below
join ∷ ∀ m a. Monad m ⇒ m (m a) → m a
join -- implementation
```

```haskell
{-# LANGUAGE UnicodeSyntax #-}

-- Not all Unicode alternatives are shown below
join ∷ ∀ m a. Monad m ⇒ m (m a) → m a
join -- implementation
```
