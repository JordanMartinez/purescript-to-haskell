# Record Syntax

PureScript has very readable Record syntax. Haskell's unfortunately sucks and is widely considered to be a "wart" in the language.

## Definition

```purescript
type EmptyRow = ()
type SingleRow = (a :: String)
type SimpleRecord = Record (a :: String)
type SimpleRecord' = Record SimpleRow

type BetterRecord = { a :: String }
```

```haskell
type Unit = ()

data PoorRecord = Record { poorRecordA :: String }

-- ^ The above is apparently syntax sugar for...
------------------------------------------------
data PoorRecord = Record String

simpleRecordA :: PoorRecord -> String
simpleRecordA (PoorRecord string) = string
------------------------------------------------
-- ... which causes namespacing issues. Thus,
-- one typically uses "qualified imports"
-- (i.e. in PureScript, we would write `import Module as M`
-- and use it via `M.functionName`) or do what I did above
-- and prefix the value with the type's name so that it
-- doesn't clash as often.
--    ~ Source: https://stackoverflow.com/a/5520803
```

## Creating a Record

### By Specifying Values

```purescript
type BetterRecord = { a :: String }

createRecord :: String -> BetterRecord
createRecord value = { a: value }
```

```haskell
data PoorRecord = Record { poorRecordA :: String }

createRecord :: String -> PoorRecord
createRecord value = Record { a = value }
```

### By Referring to Values in Context

```purescript
type TwoValueRecord = { first :: String, second :: String }

computation :: forall m. Monad m => m TwoValueRecord
computation = do
  first <- getFirst
  second <- getSecond
  pure { first, second }
```

```haskell
{-# LANGUAGE RecordWildCards #-}

data TwoValueRecord = Record { first :: String, second :: String }

computation :: forall m. Monad m => m TwoValueRecord
computation = do
  first <- getFirst
  second <- getSecond
  pure Record{..} -- beware of naming collisions
```

## Getting a field

### Using Regular Syntax
```purescript
type BetterRecord = { a :: String }

getField :: BetterRecord -> String
getField record = record.a
```

```haskell
data PoorRecord = Record { poorRecordA :: String }

getField :: PoorRecord -> String
getField record = poorRecordA record
```

### Using Annotations

```purescript
import Record (get)
import Data.Symbol (SProxy(..))

getField2 :: forall otherRows. { a :: String | otherRows } -> String
getField2 record = get (SProxy :: SProxy "a") record
```

```haskell
-- This class is defined in GHC.Records
class HasField (x :: k) r a | x r -> a where
  getField :: r -> a

-- Thus, one can use it like so:
getField2 :: HasField "a" record String => record -> String
getField2 record = getField @"a" record
```

## Updating a single field

```purescript
type AB_Record = { a :: String, b :: String }

setField :: AB_Record -> String
setField record = record { a = "some new value while B is the same" }
```

```haskell
data AB_Record = AB_Record { a :: String, b :: String }

setField :: AB_Record -> String
setField record = record { a = "some new value while B is the same" }
```

## Pattern Matching

### Match on a field and don't rebind it to a new name

```purescript
type BetterRecord = { a :: String }

patternMatch1 :: BetterRecord -> String
patternMatch1 { a } = a
```

```haskell
{-# LANGUAGE NamedFieldPuns #-}
data PoorRecord = Record { poorRecordA :: String }

-- Only possible of NamedFieldPuns extension is enabled
patternMatch1 :: PoorRecord -> String
patternMatch1 (Record { a }) = a
```

### Match on a field and bind it to a new name

```purescript
type BetterRecord = { a :: String }

patternMatch2 :: BetterRecord -> String
patternMatch2 { a: differentName } = differentName
```

```haskell
data PoorRecord = Record { a :: String }

patternMatch2 :: PoorRecord -> String
patternMatch2 (Record { a = differentName }) = differentName
```

### Match on a specific value for a given field

```purescript
patternMatch3 :: BetterRecord -> String
patternMatch3 { a: "actual value" } = "i.e. matching against a specific value"
patternMatch3 { a: allOtherValues } = allOtherValues
```

```haskell
data PoorRecord = Record { a :: String }

patternMatch3 :: PoorRecord -> String
patternMatch3 (Record { a = "actual value" }) = "i.e. matching against a specific value"
patternMatch3 (Record { a = allOtherValues }) = allOtherValues
```

### Row Polymorphism and Records

```purescript
type OpenRecord extraRows = { a :: String | extraRows }

polymorphicGet :: forall otherRows. OpenRecord otherRows -> String
polymorphicGet record = record.a

-- "value" == rowPolymorphicRecord { a: "value" }
-- "value" == rowPolymorphicRecord { a: "value", b: "still works" }
-- "value" == rowPolymorphicRecord { b: "still works", a: "value" }

polymorphicSet
  :: forall otherRows.
     String ->
     OpenRecord otherRows ->
     OpenRecord otherRows
polymorphicSet newValue record = record { a = newValue }
```

```haskell
-- Not sure whether Haskell can define an open Record type
```
