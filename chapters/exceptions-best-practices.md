This is an FP Complete coding standards document written by Michael Snoyman.
I'm exposing it to the outside world, but some of the prose definitely
maintains the coding standard approach. This piece is highly opinionated, and
I'm sure some people will have different thoughts on how to do this.

* * *

There is much debate in the Haskell community around exception handling. One
commonly stated position goes something like "all exceptions should be explicit
at the type level, and async exceptions are terrible." We can argue as much as
we want about this point in a theoretical sense. However, practically, it is
irrelevant, because GHC has already chosen a stance on this: it supports async
exceptions, and all code that runs in `IO` can have exceptions of *any* type
which is an instance of `Exception`.

As far as our coding standards go, we need to accept the world as it is, and
realize that any `IO` code can throw any exception. (We can also discuss the
theoretical benefits of the chosen setup, versus the terrible situation of
checked exceptions in Java, but that's really a separate matter.) Additionally,
all code must be written to be async-exception safe. How this is done is *not*
covered in this document.

Let's identify a few anti-patterns in Haskell exception handling, and then move
on to recommended practices.

## The bad

### ExceptT IO anti-pattern

A common (bad) design pattern I see is something like the following:

```haskell
myFunction :: String -> ExceptT MyException IO Int
```

There are (at least) three problems with this:

1. It's *non-composable*. If someone else has a separate exception type `HisException`, these two functions do not easily compose.
2. It gives an implication which is almost certainly false, namely: the only exception that can be thrown from this function is `MyException`. Almost any `IO` code in there will have the ability to throw some other type of exception, and additionally, almost any async exception can be thrown even if no synchronous exception is possible.
3. You haven't limited the possibility of exceptions, you've only added one extra avenue by which an exception can be thrown. `myFunction` can now either `throwE` or `liftIO . throwIO`.

It is almost always wrong to wrap an `ExceptT`, `EitherT`, or `ErrorT` around an `IO`-based transformer stack.

Separate issue: it's also almost always a bad idea to have such a concrete
transformer stack used in a public-facing API. It's usually better to express a
function in terms of typeclass requirements, using mtl typeclasses as
necessary.

### Mask-them-all anti-pattern

This anti-pattern goes like this: remembering to deal with async exceptions everywhere is hard, so I'll just mask them all.

Every time you do this, 17 kittens are mauled to death by the loch ness monster.

Async exceptions may be annoying, but they are vital to keeping a system
functioning correctly. The `timeout` function uses them to great benefit. The
Warp webserver bases all of its slowloris protection on async exceptions. The
cancel function from the async package will hang indefinitely if async
exceptions are masked. Et cetera et cetera.

Are async exceptions difficult to work with? Sometimes, yes. Deal with it anyway. Best practices include:

* Use the bracket pattern wherever possible.
* If you have truly complex flow of control and non-linear scoping of resources, use the resourcet package.

## The good

### MonadThrow

Consider the following function:

```haskell
foo <- lookup "foo" m
bar <- lookup "bar" m
baz <- lookup "baz" m
f foo bar baz
```

If this function returns `Nothing`, we have no idea why. It could be because:

1. "foo" wasn't in the map.
2. "bar" wasn't in the map.
3. "baz" wasn't in the map.
4. `f` returned `Nothing`.

The problem is that we've thrown away a lot of information by having our functions return `Maybe`. Instead, wouldn't it be nice if the types of our functions were:

```haskell
lookup :: Eq k => k -> [(k, v)] -> Either (KeyNotFound k) v

f :: SomeVal -> SomeVal -> SomeVal -> Either F'sExceptionType F'sResult
```

The problem is that these types don't unify. Also, it's commonly the case that
we really don't need to about *why* a lookup failed, we just need to deal with
it. For those cases, `Maybe` is better.

The solution to this is the `MonadThrow` typeclass from the exceptions package.
With that, we would write the type signatures as:

```haskell
lookup :: (MonadThrow m, Eq k) => k -> [(k, v)] -> m v
f :: MonadThrow m => SomeVal -> SomeVal -> SomeVal -> m F'sResult
```

Versus the `Either` signature, we lose some information, namely the type of
exception that could be thrown. However, we gain composability and unification
with `Maybe` (as well as many other useful instances of `MonadThrow`, like
`IO`).

The `MonadThrow` typeclass is a tradeoff, but it's a well thought out tradeoff,
and usually the right one. It's also in line with Haskell's runtime exception
system, which does not capture the types of exceptions that can be thrown.

### Transformers

The following type signature is overly restrictive:

```haskell
foo :: Int -> IO String
```

This can always be generalized with a usage of `liftIO` to:

```haskell
foo :: MonadIO m => Int -> m String
```

This allows our function to easily work with any transformer on top of `IO`.
However, given how easy it is to apply `liftIO`, it's not too horrible a
restriction. However, consider this function:

```haskell
bar :: FilePath -> (Handle -> IO a) -> IO a
```

If you want your inner function to live in a transformer on top of `IO`, you'll
find it difficult to make it work. It can be done with `lifted-based`, but it's
non-trivial. Instead, it's much better to express this function in terms of
functions from either lifted-base or exceptions, and get one of the following
more generalized type signatures:

```haskell
bar :: MonadBaseControl IO m => FilePath -> (Handle -> m a) -> m a
bar :: (MonadIO m, MonadMask m) => FilePath -> (Handle -> m a) -> m a
```

This doesn't just apply to exception handling, but also to dealing with things
like forking threads. Another thing to consider in these cases is to use the
`Acquire` type from resourcet.

### Custom exception types

The following is bad practice:

```haskell
foo = do
    if x then return y else error "something bad happened"
```

The problem is the usage of arbitrary string-based error messages. This makes
it difficult to handle this exceptional case directly in a higher level in the
call stack. Instead, despite the boilerplate overhead involved, it's best to
define a custom exception type:

```haskell
data SomethingBad = SomethingBad
    deriving Typeable
instance Show SomethingBad where
    show SomethingBad = "something bad happened"
instance Exception SomethingBad
foo = do
    if x then return y else throwM SomethingBad
```

Now it's trivial to catch the `SomethingBad` exception type at a higher level.
Additionally, `throwM` gives better exception ordering guarantees than `error`,
which creates an exception in a pure value that needs to be evaluated before
it's thrown.

One sore point is that some people strongly oppose a `Show` instance like this.
This is an open discussion, but for now I believe we need to make the tradeoff
at this point in the spectrum. I've proposed to the libraries mailing list to
add a new method to the `Exception` typeclass used for user-friendly display of
exceptions, which will make this less of a sore point.

## See also

* [Catching all exceptions](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions)
* [Exceptions and monad transformers](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers)
* [Exceptions in continuation-based monads](http://www.yesodweb.com/blog/2014/05/exceptions-cont-monads)
* [A Stack Overflow answer on this subject](http://stackoverflow.com/questions/25752900/exceptions-and-monad-transformers/25753497#25753497)
