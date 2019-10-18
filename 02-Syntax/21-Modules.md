# Modules

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
