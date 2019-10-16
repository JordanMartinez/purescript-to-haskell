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

## The `data` Keyword

These are the same.

```purescript
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
```

## The `newtype` Keyword

These are the same

```purescript
newtype Age = Age OriginalType
```

```haskell
newtype Age = Age OriginalType
```

## The `type` keyword

### Basic

These are the same
```purescript
type TypeAlias = OriginalType
```

```haskell
type TypeAlias = OriginalType
```

### Using `forall` in the Type Alias Declaration

PureScript's works out-of-box. Haskell requires a language extension called `LiberalTypeSynonyms`

```purescript
type ProduceSomeB b = forall a. a -> b
```

```haskell
{-# LANGUAGE LiberalTypeSynonyms #-}
type ProduceSomeB b = forall a. a -> b
```


## Empty Data Types

PureScript can define data types that don't have a constructor (usually for phantom types that tag something at the type-level). Haskell requires the `EmptyDataDecl` language extension to be enabled.

```purescript
data PhantomType_NoConstructors
```

```haskell
{-# LANGUAGE EmptyDataDecl #-}

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

#### Basic

Purescript uses `[]` syntax for Arrays. Haskell uses `[]` syntax for `List`s.

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
import Data.Vector (generate) -- i.e. `forall a. Int -> (Int -> a) -> Vector a`

-- type List a = [a]

intList :: [Int] -- i.e. `List Int`
intList = [1, 2, 3] -- i.e. 1 : 2 : 3 : Nil where `:` is `Cons`

intArray :: Vector Int
intArray = generate 4 id -- i.e. `id :: forall a. a -> a`
```

#### Additional Comparisons and Defining Ranges

In PureScript, we can define a range of values using a function. Haskell has syntax directly for this:

```pureScript
[]          -- Empty Array
[x]         -- Array with 1 element
[x,y,z]     -- Array with 3 elements

-- Not supported
-- [x .. ]     -- enumFrom x
-- [x,y ..]    -- enumFromThen x y

x .. y    -- `x .. y` is infix for `range x y` from `Data.Array (range)`

-- Not supported
-- [x,y .. z]  -- enumFromThenTo x y z
```

[These examples are taken from the examples in `OverloadedLists` extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedLists)
```haskell
[]          -- Empty list
[x]         -- x : []
[x,y,z]     -- x : y : z : []
[x .. ]     -- enumFrom x
[x,y ..]    -- enumFromThen x y
[x .. y]    -- enumFromTo x y
[x,y .. z]  -- enumFromThenTo x y z
```

#### The `OverloadedLists` Language Extension

Fortunately, Haskell can enable the `OverloadedLists` extension to make the `[]` syntax work on more things. Here's how it works. There's a type clas called `IsList`:

```haskell
class IsList l where
  type Item l

  fromList :: [Item l] -> l
  toList   :: l -> [Item l]

  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList
```

When the extension is enabled, the above examples are desugared to this:

```haskell
[]          -- fromListN 0 []
[x]         -- fromListN 1 (x : [])
[x,y,z]     -- fromListN 3 (x : y : z : [])
[x .. ]     -- fromList (enumFrom x)
[x,y ..]    -- fromList (enumFromThen x y)
[x .. y]    -- fromList (enumFromTo x y)
[x,y .. z]  -- fromList (enumFromThenTo x y z)
```

Thus, one can use this syntax to write:
```haskell
['0' .. '9']             :: Set Char
[1 .. 10]                :: Vector Int
[("default",0), (k1,v1)] :: Map String Int
['a' .. 'z']             :: Text
```

Returning to our previous example, we could write:
```haskell
{-# LANGUAGE OverloadedLists #-}
import Data.Vector

intList :: [Int] -- i.e. `List Int`
intList = [1, 2, 3] -- i.e. 1 : 2 : 3 : Nil where `:` is `Cons`

intArray :: Vector Int
intArray = [1, 2, 3]
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

Similar to the `OverloadedLists` extension above, the double-quote syntax can be overridden so that it does not refer to Haskell's `[Char]` type. Here's how it works. There's a type class called `IsString`:

```haskell
class IsString a where
    fromString :: String -> a
```

When `OverloadedStrings` is enabled, this syntax `"text"` becomes `fromString "text"`. As a result, we can rewrite the above `Text` and `ByteString` examples as:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text

realStringValue :: Text
realStringValue =
  "The output of `pack` will be the String data type you're used to."
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString

binaryStringValue :: ByteString
binaryStringValue = "a string value"
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

#### Basic

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

#### Underscore Syntax Sugar for Ints and Numbers

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

#### Dealing with Negated Literals

PureScript handles negative literals properly. Haskell requires parenthesis. One can remove this by enabling [the `NegativeLiterals` language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NegativeLiterals). See the extension for what else it affects.

```purescript
repl> 4 + -4
0
```

```haskell
repl> 4 + -4
  Precedence parsing error
    cannot mix ‘+’ [infixl 6] and prefix `-' [infixl 6] in the same
    infix expression
repl> 4 + (-4)
0
repl> :set -XNegativeLiterals
repl> 4 + -4
0
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
