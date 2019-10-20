# Lazy by Default

## Converting "strict by default" code to "lazy by default" code

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

Depending on your perspective, Haskell is for better or worse lazy by default. Thus, if we converted the above PureScript code into Haskell and did not force the code to be strict, this is how the equivalent Haskell code would look in PureScript. In short, all values are wrapped in a thunk/closure:
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

Obviously, the above program would not be as performant as it could be. Knowing when, where, and how to make Haskell strict is often a challenge new learners face.

## Haskell's Laziness

### Pros and Cons of Laziness

In a blog post to which I will link later in this file, Michael Snoyman says that "laziness is sometimes an asset, and sometimes a liability."

For a summary of the benefits of laziness, see [this StackOverflow answer](https://stackoverflow.com/a/265548/4846512).

The two main issues that can arise with laziness are:
1. space leaks - [A more formal definition](https://stackoverflow.com/a/46007799/4846512)
2. unpredictable runtime behavior/performance

### Thunks vs Weak Head Normal Form vs Normal Form

Values in Haskell are in one of three forms:
- unevaluated thunks
- Weak Head Normal Form (WHNF)
- Normal Form (NF)

There is another concept known as Head Normal Form (HNF), but Haskell does not use this.

The following StackOverflow answer provides [a visualization describing the difference between thunks, Weak Head Normal Form (WHNF), and Normal Form (NF)](https://stackoverflow.com/a/9342882).

### Avoiding the Negative Aspects of Laziness

Read [All about Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness) to understand how Haskell's laziness works and how to avoid the negative side of it.

The rest of this section summarizes his points and links to other things that were mentioned in the above post or its Reddit thread:
- 4 key functions (the last one was mentioned in the Reddit thread and Snoyman didn't include it in his post)
    - `seq first second` means that after evaluating `seq` both `first` and `second` will have been evaluated to Weak-Head Normal Form. However, GHC is free to evaluate these in whichever order it chooses. If one's code is pure, one will not notice a difference. If one uses impure code (e.g. `trace`), then one can observe the difference. See [the documentation on `seq`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:seq)
    - `deepseq first second` works the same as `sea` except that both `first` and `second` will have been evaluated to Normal Form rather than Weak Head Normal Form. A runtime exception will occur if `first`, `second`, or any of their contained thunks will produce a runtime exception when evaluated. See [the documentation on `deepseq`](https://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html#v:deepseq)
    - `force (some expression)` means "when `some expression` is evaluated to Weak-Head Normal Form, evaluate it to Normal Form instead. See [the documentation on `force`](https://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html#v:force)
    - `pseq first second` works the same as `seq` but guarantees that `first` is evaluated before `second` because its first argument is strict and its second argument is lazy. See [the documentation on `pseq`](https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel.html#v:pseq).
- [`BangPatterns`] are a language extension that makes `!` act as syntax sugar for `seq`. However, some expressions cannot be expressed using that. So, one should still famliarize themselves with `seq`.
- Usage of BangPatterns (i.e. `!`) and what they mean in various contexts:
    - In a pattern match (e.g. `foo (Container !value) = --`) means "Evaluate this value to WHNF when a pattern match occurs".
    - In a let binding (e.g. `let !value = --`) means "Evaluate this value to WHNF before its used anywhere else."
    - In a data declaration (e.g. `data Foo = Foo !Int`) means "Whenever you evaluate a value of type `Foo` to WHNF (e.g. pattern match on it), you must also evaluate the strict-annotated fields it contains to WHNF" (edited but I'm basically quoting Snoyman here).
- > You're probably asking a pretty good question right now: "how do I know if I should use a strictness annotation on my data fields?" This answer is slightly controversial, but my advice and recommended best practice: **unless you know that you want laziness for a field, make it strict.**
- Given `newtype Foo = Foo Int`, calling `case Foo undefined of {Foo _ -> ...}` is the same as calling `case undefined of {_ -> ...}` because `Foo` only exists at compile-time, not runtime. `undefined` is never evaluated/observed due to using `_` so the program doesn't crash.
- The `$` functions:
    - `f $ a` is the same as `f a`
    - `f $! a` is the same as [`f weakHeadNormalFormA`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:-36--33-)
    - `f $!! a` is the same as [`f normalFormA`](https://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html#v:-36--33--33-)
- Strictness of data structures
    - "spine strict" = the "structure" of the data type can be evaluated without evaluating any of its values. I think this tends to occur with recursive data types like `List`.
    - "value strict" = the values in a data type are strict. If any of them contain an `undefined`, they will produce a runtime error when evaluated.
- Lazy fold left (i.e. `foldl`) vs Strict fold left (i.e. `foldl'`)
    - It tends to be safer to use `foldl'` rather than `foldl`. The former is the strict version whereas the latter is the lazy version.
    - [You cannot have any control over strictness with `foldl` but you can have complete control over strictness with `foldl'`](https://www.reddit.com/r/haskell/comments/6zl88c/all_about_strictness/dmwd8z2/)
- "You can build up as many chains of evaluation using `seq` and `deepseq` as you want in your program. But ultimately, unless you force evaluation via some `IO` action of the value at the top of the chain, it will all remain an unevaluated thunk."

## A Guide to Fixing Space Leaks

Finding and fixing space leaks seems to require using profiler tools. See [Leaking Space](https://queue.acm.org/detail.cfm?id=2538488) to understand the concept better and tips for fixing them.
