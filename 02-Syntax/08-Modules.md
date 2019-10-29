# Modules

## Location of Pragmas

Not all Pragmas go here (e.g. INLINE, etc.), but this is where language extensions and GHC options go if they are enabled for the given file or override default values for the given file.

```Haskell
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ModuleName
  ( exportedFunction
  , exportedValue
  ) where

import ModuleName

exportedFunction :: Show a => a -> String
exportedFunction = show

exportedValue :: Int
exportedValue = 4
```

## Importing

Since PureScript does not allow Orphan Instances, type class instances are imported if they are used in the module. Haskell requires importing the module where the instance is defined. <span style="color: red;">*</span>

| PureScript Idea | PureScript Syntax | Haskell Syntax | Haskell Explanation
| - | - | - | - |
| import all exported entities from a module | `import Foo` | `import Foo` | Same as PureScript, except one can also use `Foo.function` to refer to `function`. |
| import only specific exported entities from a module | `import Foo(bar)` | `import Foo(bar)` | Same as PureScript, except one can also use `Foo.function` to refer to `function`. |
| import all exported entities from a module, but references to them MUST begin with `F` (e.g. `F.function`) | `import Foo as F` | `import qualified Foo as F` | Same as PureScript |
| -- | -- | `import Foo as M` | import all exported entities from a module. One can refer to a function via `function` or via `M.function`. (Useful when only some names clash between two modules) |
| -- | -- | `import Foo as M (bar)` | import only specific entities from a module. One can refer to a function via `bar` or via `M.bar`. (Useful when only some names clash between two modules) |
| -- | -- | `import Foo ()` | <span style="color: red;">*</span> import nothing except type class instances from a module. |

See Haskell's table that summarizes its imports in the [HaskellWiki's `Import` page](https://wiki.haskell.org/Import).

### Normal Imports

```purescript
-- import values from a module
import ModuleValues (value1, value2)

-- imports functions from a module
import ModuleFunctions (function1, function2)

-- imports function alias from a module
import ModuleFunctionAliases ((/=), (===), (>>**>>))

-- imports type class from the module
import ModuleTypeClass (class TypeClass)

-- import a type but none of its constructors
import ModuleDataType (DataType)

-- import a type and one of its constructors
import ModuleDataType (DataType(Constructor1))

-- import a type and some of its constructors
import ModuleDataType (DataType(Constructor1, Constructor2))

-- import a type and all of its constructors
import ModuleDataType (DataType(..))

-- import a type alias
import ModuleTypeAlias (type MyTypeAlias)

-- import a kind and its value
import ModuleKind (kind ImportedKind, ImportedKindValue)

-- import a type class instance
--    nothing needs to be done here.
```

```haskell
-- import values from a module
import ModuleValues (value1, value2)

-- imports functions from a module
import ModuleFunctions (function1, function2)

-- imports function alias from a module
import ModuleFunctionAliases ((/=), (===), (>>**>>))

-- imports type class from the module
import ModuleTypeClass (TypeClass)

-- import a type but none of its constructors
import ModuleDataType (DataType)

-- import a type and one of its constructors
import ModuleDataType (DataType(Constructor1))

-- import a type and some of its constructors
import ModuleDataType (DataType(Constructor1, Constructor2))

-- import a type and all of its constructors
import ModuleDataType (DataType(..))

-- import a kind and its value
import ModuleKind (ImportedKind, ImportedKindValue)

-- import nothing but type class instances
import ModuleWithInstances ()
```

### Dealing with Entity Name Clashes

#### Basic

```purescript
-- resolve name conflicts using "hiding" keyword
import ModuleNameClash1 (sameFunctionName1)
import ModuleNameClash2 hiding (sameFunctionName1)

-- resolve name conflicts using module aliases
import ModuleNameClash1 as M1
import ModuleNameClash2 as M2
```

```haskell
import ModuleNameClash1 (sameFunctionName1)
import ModuleNameClash2 hiding (sameFunctionName1)

-- resolve name conflicts using module aliases
import qualified ModuleNameClash1 as M1
import qualified ModuleNameClash2 as M2
```

#### Package Imports

Haskell also allows one to use the [`PackageImports`](https://limperg.de/ghc-extensions/#packageimports) language extension to import modules with the same name.

### Importing Type Aliases that Use Symbolic Names

PureScript's works out-of-box whereas Haskell must enable the [`ExplicitNamespaces` extension](https://limperg.de/ghc-extensions/#explicitnamespaces)

```purescript
{-
-- i.e. in another file...
module ModuleTypeAlias (type (~>)) where

type NaturalTransformation f g  = forall a. f a -> f g
infixr 4 type NaturalTransformation as ~>
-}

-- import a type alias that uses infix notation
import ModuleTypeAlias (type (~>))
```

```haskell
{-# LANGUAGE ExplicitNamespaces #-}
import ModuleTypeAlias (type (~>))
```

## Exporting

Haskell syntax below has not yet been tested.

```purescript
module ExampleExports
  -- exports go here by just writing the name
  ( value

  , function, (>@>>>) -- aliases must be wrapped in parenthesis

  -- when exporting type classes, there are two rules:
  -- - you must precede the type class name with the keyword 'class'
  -- - you must also export the type class' members (or face compilation errors)
  , class TypeClass, tcFunction

  -- when exporting modules, you must precede the module name with
  -- the keyword 'module'
  , module ExportedModule

  -- The type is exported, but no one can create an value of it
  -- outside of this module
  , ExportDataType1_ButNotItsConstructors

  -- The type is exported and only one of its constructors is exported. Thus,
  -- everyone else can create a `Constructor2A' value but not a
  -- `Constructor2B` value. That one can only be created inside this module.
  , ExportDataType2_AndOneOfItsConstructors(Constructor2A)

  -- The type is exported and some of its constructors are exported. Thus,
  -- everyone else can create a `Constructor3A' value
  -- and a `Constructor3B` value, but not a `Constructor3C` value, which
  --  can only be created inside this module.
  , ExportDataType3_AndSomeOfItsConstructors(Constructor3A, Constructor3B)

  , ExportDataType4_AndAllOfItsConstructors(..) -- syntax sugar for 'all constructors'

  -- Type aliases can also be exported
  , ExportedTypeAlias

  -- When type aliases are aliased using infix notation, one must export
  -- both the type alias, and the infix notation where 'type' must precede
  -- the infix notation
  , ExportedTypeAlias_InfixNotation, type (<|<>|>)

  -- Data constructor alias; exporting the alias requires you
  -- to also export the constructor it aliases
  , ExportedDataType4_InfixNotation(Infix_Constructor), (<||||>)

  , module Exports

  -- Kinds require the `kind` keyword to precede them
  , kind ExportedKind
  , ExportedKindValue
  ) where

-- Re-export modules
import Module1 (anInt1) as Exports
import Module2 (anInt2) as Exports
import Module3 (anInt3) as Exports
import Module4.SubModule1 (someFunction) as Exports

import ExportedModule
```

```haskell
module ExampleExports
  -- exports go here by just writing the name
  ( value

  , function, (>@>>>) -- aliases must be wrapped in parenthesis

  -- when exporting type classes, there are two rules:
  -- - you must precede the type class name with the keyword 'class'
  -- - you must also export the type class' members (or face compilation errors)
  , class TypeClass, tcFunction

  -- when exporting modules, you must precede the module name with
  -- the keyword 'module'
  , module ExportedModule

  -- The type is exported, but no one can create an value of it
  -- outside of this module
  , ExportDataType1_ButNotItsConstructors

  -- The type is exported and only one of its constructors is exported. Thus,
  -- everyone else can create a `Constructor2A' value but not a
  -- `Constructor2B` value. That one can only be created inside this module.
  , ExportDataType2_AndOneOfItsConstructors(Constructor2A)

  -- The type is exported and some of its constructors are exported. Thus,
  -- everyone else can create a `Constructor3A' value
  -- and a `Constructor3B` value, but not a `Constructor3C` value, which
  --  can only be created inside this module.
  , ExportDataType3_AndSomeOfItsConstructors(Constructor3A, Constructor3B)

  , ExportDataType4_AndAllOfItsConstructors(..) -- syntax sugar for 'all constructors'

  -- Type aliases can also be exported
  , ExportedTypeAlias

  -- When type aliases are aliased using infix notation, one must export
  -- both the type alias, and the infix notation where 'type' must precede
  -- the infix notation
  , ExportedTypeAlias_InfixNotation, type (<|<>|>)

  -- Data constructor alias; exporting the alias requires you
  -- to also export the constructor it aliases
  , ExportedDataType4_InfixNotation(Infix_Constructor), (<||||>)

  , module Exports

  -- I think kinds get exported like `data` exports
  , ExportedKind
  , ExportedKindValue
  ) where

-- Re-export modules
import Module1 (anInt1) as Exports
import Module2 (anInt2) as Exports
import Module3 (anInt3) as Exports
import Module4.SubModule1 (someFunction) as Exports

import ExportedModule
```
