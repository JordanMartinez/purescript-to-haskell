# Lazy by Default

PureScript is strict by default (a constraint imposed by the JavaScript backend). Thus, when we define data types and functions like below...
```purescript
data Foo = Foo Int String

type Stuff =
  { myInt :: Int
  , myBool :: Boolean
  }

fooToStuff :: Foo -> Stuff
fooToStuff (Foo i s) = { myInt: i, myBool: s == "meh" }

stuffRender :: Stuff -> String
stuffRender record = (show record.myInt) <> " , " <> (show record.myBool)

main :: Effect Unit
main = log (render (fooToStuff (Foo 4 "no")))
```
... we evaluate `Foo 4 "no"` right away to produce a `Foo`, then pass it into `fooToStuff`, which converts it to `Stuff` immediately, and `show` converts it into a `String` and `log` prints that value to the console.

Haskell is lazy by default. Thus, if we converted the above PureScript code into Haskell and did not force anything to be strict, this is how it would look in PureScript. In short, all values are wrapped in a thunk/closure:
```purescript
-- A closure, a thunk, or whatever you want to call it
type Lazy a = Unit -> a

force :: Lazy a -> a
force thunk = thunk unit

data Foo = Foo (Lazy Int) (Lazy String)

type Stuff =
  { myInt :: Lazy Int
  , myBool :: Lazy Boolean
  }

main :: Effect Unit
main =
  let
    -- First it builds up a ton of closures
    lazyInt = \_ -> 4
    lazyString = \_ -> "no"

    -- PS' Foo data constructor is strict,
    -- so I use this syntax to show that it's being constructed lazily
    _Foo = \_ -> Foo lazyInt lazyString

    fooToStuff = \_ -> case force _Foo of
      Foo lazyInt lazyString -> { myInt: lazyInt
                                , myBool: \_ -> (force lazyString) == "meh"
                                }
    stuffRender = \_ ->
      let
        record = force fooToStuff
        realInt = force record.myInt
        realBool = force record.myBool
      in
        -- Technically, `show` here would be another thunk that gets forced
        -- or evaluated (e.g. `force (show realInt)`), but it was easier
        -- to provide this example if it operated in a strict manner.
        (show realInt) <> " , " <> (show realBool)

    log = \_ -> force stuffRender

  in
    -- Then it gets to the last one and finally forces the thunk /
    -- evaluates the closure. Each in turn forces the
    -- next thunk / evaluates the next closure until
    -- the program finshes executing.
    force log
```

Read [All about Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness) to understand how Haskell's laziness can work unexpectedly.
