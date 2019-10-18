# Type Classes

## Defining Type Classes

### Single Parameter Type Classes

#### Basic

These are the same for both

```purescript
class TypeClassName parameterType where
  functionName :: parameterType -> ReturnType

  valueName :: ValueType
```

```haskell
class TypeClassName parameterType where
  functionName :: parameterType -> ReturnType

  valueName :: ValueType
```

#### Relationships

PureScript's arrow faces towards the required class (i.e. `<=`).
Haskell's arrow faces towards the actual class (i.e. `=>`).

```purescript
                      -- |  | - the arrow faces towards required class
class RequiredTypeClass a <= ActualTypeClass a where
  functionName :: a -> ReturnType

class (RequiredTypeClass1 a, RequiredTypeClass2 a {-, ... -}) <= TheTypeClass a where
  function :: a -> a
```

```haskell
                      -- |  | - the arrow faces towards actual class
class RequiredTypeClass a => ActualTypeClass a where
  functionName :: a -> ReturnType

class (RequiredTypeClass1 a, RequiredTypeClass2 a {-, ... -}) => TheTypeClass a where
  function :: a -> a
```

### Empty Type Classes

Haskell requires you to enable the `MultiParameterTypeClasses` language extension to define an empty type class. PureScript's definition works out-of-box.

```purescript
class EmptyTypeClass
```

```haskell
{-# LANGUAGE MultiParameterTypeClasses #-}
class EmptyTypeClass
```

### Type Class Members with Constraints

PureScript can add type class constraints to its members out-of-box. Haskell requires enabling a language extension called `ConstrainedClassMethods`:

```purescript
class SomeClass a where
  someFunction :: Eq a => a -> a -> a
```

```haskell
{-# LANGUAGE ConstrainedClassMethods #-}

class SomeClass a where
  someFunction :: Eq a => a -> a -> a
```

### Other Extensions to Enable by Default

- `FlexibleInstances`
- `FlexibleContexts`

### Multiple Parameters

#### Without Functional Dependencies

Haskell requires you to enable the `MultiParameterTypeClasses` language extension to define a type class with multiple parameters. PureScript's definition works out-of-box.

```purescript
class MultiParameterTypeClass type1 type2 {- typeN -} where
  functionName1 :: type1 -> type2 -> {- typeN -> -} ReturnType
```

```haskell
{-# LANGUAGE MultiParameterTypeClasses #-}
class MultiParameterTypeClass type1 type2 {- typeN -} where
  functionName1 :: type1 -> type2 -> {- typeN -> -} ReturnType
```

#### With Functional Dependencies

PureScript's definition works out-of-box. Haskell requires you to enable the `FunctionalDependencies` language extensions to define a type class with functional dependencies between its multiple parameters. In addition, you might need to enable the `UndecidableInstances` language extension when using functional dependencies; otherwise, the compiler might think your instances are invalid.

Note: enabling Haskell's `FunctionalDependencies` language extension implies enabling the `MultiParameterTypeClasses` language extension.

```purescript
class ManyTypesDetermineAnotherType a b c | a b {- n -} -> c  where
  functionName2 :: a b -> c

class OneTypeDeterminesManyTypes a b c | a -> b c where
  functionName3 :: a -> b c

class ManyFDRelationships a b c | a b -> c, c -> a b where
  functionName4 :: a -> b -> c
```

```haskell
{-# LANGUAGE FunctionalDependencies #-}
-- ^ implies {-# LANGUAGE MultiParameterTypeClasses #-}

class ManyTypesDetermineAnotherType a b c | a b {- n -} -> c  where
  functionName2 :: a b -> c

class OneTypeDeterminesManyTypes a b c | a -> b c where
  functionName3 :: a -> b c

class ManyFDRelationships a b c | a b -> c, c -> a b where
  functionName4 :: a -> b -> c
```

### Using Kind Signatures

PureScript's kind signatures work out-of-box. Haskell requires a language extension called `KindSignatures`.

```purescript
class TypeLevelFunction (input :: InputKind) f | input -> f
```

```haskell
{-# LANGUAGE KindSignatures #-}

class TypeLevelFunction (input :: InputKind) f | input -> f
```

## Constraint Kinds

### Context

Let's say we have the following function:
```purescript
foo
  :: forall m.
     MonadReader Config m =>
     MonadState State m =>
     MonadWriter String m =>
     MonadCatch Error m =>
     m Unit
foo = ...
```

If we have to define 6 other functions that use those same type class constraints, it can lead to a lot of boilerplate. "Constraint kinds" provide a solution to this problem.

### Comparison

Unfortunately, PureScript does not yet allow one to define a constraint kind. However, Haskell does if one enables [the `ConstraintKinds` language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstraintKinds)

```purescript
{-
This pseudo-code does not really work

type ConstraintKind m a =
  MonadReader Config m =>
  MonadState State m =>
  MonadWriter String m =>
  MonadCatch Error m =>
  m a

foo :: ConstraintKind m Unit
foo = ...

-}
```

```haskell
{-# LANGUAGE ConstraintKinds #-}

type ConstraintKind m =
  ( MonadReader Config m
  , MonadState State m
  , MonadWriter String m
  , MonadCatch Error m
  )

foo :: ConstraintKind m => m Unit
foo = ...
```

## Defining Type Class Instances

### Single Parameter Instances

#### Basic

PureScript requires you to name your instance. I think this is a constraint imposed by the JavaScript runtime. Haskell does not.

```purescript
data DataTypeName = DataTypeName

class TypeClassName a where
  stringMe :: a -> String

-- Name needed for the instance (JavaScript-imposed constraint)
-- Names follow a convention: "typeClassNameDataTypeName"
instance nameOfInstance :: TypeClassName DataTypeName where
  stringMe _ = "DataTypeName"
```

```haskell
data DataTypeName = DataTypeName

class TypeClassName a where
  stringMe :: a -> String

-- No name needed for the instance
instance TypeClassName DataTypeName where
  stringMe _ = "DataTypeName"
```

#### Including Type Signatures in the Instances

PureScript allows you to include the type signature of the type class member you are implementing. Haskell only allows this if the `InstanceSigs` extension is enabled.

```purescript
data DataTypeName = DataTypeName

class TypeClassName a where
  stringMe :: a -> String

-- Including the type signature is allowed
instance TypeClassNameDataTypeName :: TypeClassName DataTypeName where
  stringMe :: DataType -> String
  stringMe _ = "DataTypeName"
```

```haskell
{-# LANGUAGE InstanceSigs #-}
data DataTypeName = DataTypeName

class TypeClassName a where
  stringMe :: a -> String

instance TypeClassName DataTypeName where
  stringMe :: DataType -> String
  stringMe _ = "DataTypeName"
```

#### Instances Requiring Other Instances

These work the same in both languages

```purescript
instance showMaybe :: (Show a) => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just(" <> show a <> ")"
```

```haskell
instance (Show a) => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just(" <> show a <> ")"
```

### Multiple Paramter Instances

```purescript
class Multi one two three where
  multiFunction :: one -> two -> three -> String

instance multiIntBooleanChar :: Multi Int Boolean Char where
  multiFunction i b c = "done"
```

```haskell
{-# LANGUAGE MultiParameterTypeClasses #-}
class Multi one two three where
  multiFunction :: one -> two -> three -> String

instance Multi Int Boolean Char where
  multiFunction i b c = "done"
```

### Instance Chains

PureScript allows the usage of "type class instance chains" to help determine which instance should be used. Haskell [does not appear to have this feature implemented](https://gitlab.haskell.org/ghc/ghc/issues/9334), but there are various ways to get around that. I'm not yet sure what those ways are besides those mentioned in the above link.

Regardless, [the PureScript community strongly opposes allowing orphan instances](https://github.com/purescript/purescript/issues/1247#issuecomment-512975645) as illustrated by Harry's comment.

```purescript
class DoSomething a where
  doSomething :: a -> String

-- Readability note: some put `else instance` on the same line.
-- Others put these on separate lines.
--    else
--    instance
-- I show both below.
instance firstTry :: DoSomething Int where
  doSomething = "an int"
else instance secondTry :: DoSomething Number where
  doSomething = "a number"
else
instance thirdTry :: DoSomething String where
  doSomething = "a string"
else
instance catchall :: DoSomething a where
  doSomething = "some unknown value"
```

```haskell
-- TODO: not sure how Haskell does this yet.
```

## Deriving Type Class Instances

### Non-Newtyped Types

PureScript's compiler can derive instances for the `Eq`, `Ord`, and `Functor` type classes. For other type classes, it uses the `Generic` type class.

Haskell can derive many type classes in a more consise manner than PureScript.

#### Eq and Ord

PureScript and Haskell can derive instances for `Eq` and `Ord`, but Haskell's syntax is more concise.

```purescript
data SomeType = SomeType

derive instance eqSomeType :: Eq SomeType
derive instance ordSomeType :: Ord SomeType
```

```haskell
data SomeType = SomeType
  deriving (Eq, Ord)
```

#### Functor

PureScript can derive a functor out-of-box as long as the type has the necessary shape. Haskell requires enabling the `DeriveFunctor` language extension.

```purescript
data SomeFunctor a = SomeFunctor a
derive instance functorSomeFunctor :: Functor SomeFunctor
```

```haskell
{-# LANGUAGE DeriveFunctor #-}
data SomeFunctor a = SomeFunctor a
  deriving (Functor)
```

#### Other Type Classes

PureScript uses the `Generic` type class to derive default implementations for the following type classes:
- via `purescript-generics-rep`:
    - Show
    - Eq
    - Ord
    - Bounded
    - Enum
    - Heyting Algebra
    - Semigroup
    - Monoid
    - Semiring
    - Ring
- via `purescript-argonaut-generic`:
    - EncodeJson
    - DecodeJson

```purescript
data SomeType = SomeType

derive instance genericSomeType :: Generic (SomeType a) _

instance showSomeType :: Show SomeType where
  show x = genericShow x
```

Haskell can derive instances for the following type classes:
- Show
- Eq
- Ord
- Bounded
- Enum
- Read (just FYI. Don't use this type class (source: "Haskell From First Principles" book)

Note: Haskell's numerical type class hierarchy works differently than PureScript's. We'll cover that later.

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

data SomeFunctor a = SomeFunctor a
  deriving (
    Eq, Ord, Show, Bounded, Enum,
    -- ^ These don't require any language extension to be enabled...
    Functor,
    -- (Satisfy Traversable's dependency upon Functor)
    Foldable,
    -- ^ only if `DeriveFoldable` extension is enabled
    Traversable,
    -- ^ only if `DeriveTraversable` extension is enabled
  )
```

### Newtyped Types

PureScript can derive an instance if the type that was newtyped has an instance for it. Haskell can do the same only if the `GeneralizedNewtypeDeriving` language extension is enabled. In addition, one might need to enable the `DerivingVia` extension to ensure the correct instance was used.

```purescript
class SpecialShow a where
  specialShow :: a -> String

instance SpecialShow Int where
  specialShow x = show (x * 100)

newtype Cent = Cent Int
derive newtype instance specialShowCent :: SpecialShow Cent
```

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class SpecialShow a where
  specialShow :: a -> String

instance SpecialShow Int where
  specialShow x = show (x * 100)

newtype Cent = Cent Int
  deriving (SpecialShow)
```

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

-- Given these two type classes
class Semigroup a where
  append :: a -> a -> a

class Semigroup a => Monoid m where
  mempty :: a

-- and these two implementations
newtype Sum = Sum Int
newtype Product = Product Int

instance Semigroup Sum where append = (+)
instance Monoid Sum where mempty = 0

instance Semigroup Product where append = (*)
instance Monoid Product where mempty = 1

-- we can specify that the Semigroup instance uses the Sum Int one,
-- not the Product Int one.
newtype Cent = Cent Int
  deriving Semigroup via (Sum Int)
```
