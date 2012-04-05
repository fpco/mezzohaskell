Conduit
=======

There are a number of other documents out there discussing [conduit](http://hackage.haskell.org/package/conduit)s. However,
they are all trying to explain how conduit works, or why it's necessary, or how
it's different from enumerator/pipes, or debating design decisions. This is
meant to be the authoritative guide to getting stuff done.

The goal of conduit is to allow efficient processing of data streams with
deterministic resource handling. You can consider it as an alternative to lazy
I/O. Instead of working up a motivation for why you would use conduit, I'll let
the examples below speak for themselves.

We'll start off with some ridiculously simple examples. I'm not actually
recommending that you use conduits in place of normal folds, the simple
examples are just there to build up an intuition before jumping into the real
use cases.

Sources, Sinks, and connecting
==============================

The simplest usage of the `conduit` package will involve two types: a `Source`
and a `Sink`. As you might guess, a `Source` produces a stream of values, and a
`Sink` consumes that stream. In order to connect them and get a result, we use
the connect operator, `$$`. Let's see an example:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
    list <- CL.sourceList [1..10] $$ CL.consume
    mapM_ print list
```

We imported two modules. `Data.Conduit` contains the basics of the library.
`Data.Conduit.List` contains a number of functions that let us interact with
`conduit` as we would with lists (we'll see examples later). Due to name
clashes, we normally import the module qualified.

We're using two functions from `Data.Conduit.List`: `sourceList` produces a new
`Source` from a list of values, and `consume` is a `Sink` which will consume a
stream of value into a list. As you might guess, the output of this program is
the numbers 1 to 10.

This doesn't seem like a very efficient approach: we need to read the entire
list into memory before we do anything with it. Instead, it would be better to
have our `Sink` print the values for us. Fortunately, there's an easy way to do
this:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = CL.sourceList [1..10] $$ CL.mapM_ print
```

The output from this program is identical to the first one, but this is far
more efficient. As the `Source` provides each new value to the `Sink`, the
`Sink` immediately prints it, and then discards the value. In other words, this
program runs in constant memory.

Let's see another example of constant memory consumption:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
    sum <- CL.sourceList [1..10] $$ CL.fold (+) 0
    print sum
```

As you (hopefully) guessed, this prints out 55. As a seasoned Haskeller, you
may be wondering: was that a left fold, or a right fold? Answer: it's a left
fold. We can't implement a right fold in `conduit` without consuming the entire
stream into memory. Additionally, this is a *strict* left fold, a.k.a `foldl'`.

If you have a use case where you would ordinarily want to use a `foldr`, you'll
most likely need to rethink the approach, possibly by using a `Conduit`
(covered later).

Monads
======

If you look back at our previous examples, they all produced results in the
`IO` monad. This doesn't have to be the case: `conduit` will work with any
underlying monad. In pure code, you might want to use `Identity`:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Functor.Identity

main :: IO ()
main = do
    let sum = runIdentity $ CL.sourceList [1..10] $$ CL.fold (+) 0
    print sum
```

Or if you want to be *really* clever, you can just use something like the
`Monad` instance for functions:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Functor.Identity

main :: IO ()
main = do
    let sum = (CL.sourceList [1..10] $$ CL.fold (+) 0) ()
    print sum
```

`conduit` has been designed from the ground up to work with any arbitrary
`Monad`, and especially `Monad` stacks.

Resource allocation
-------------------

We started off by saying that `conduit` is an alternative to lazy I/O. Let's
look at a simple program using lazy I/O: copying one file to another:

```haskell
main :: IO ()
main = readFile "input.txt" >>= writeFile "output.txt"
```

The problem with this kind of code is that we have no guarantees of when file
handles will be closed. In a program this small, it doesn't matter. But in
larger programs, like a web server serving thousands of requests per second,
that uncertainty can be a problem.

Fortunately, `conduit` comes built in with a pair of functions that mirror
`readFile` and `writeFile`: `sourceFile` and `sinkFile`. They are provided by
the `Data.Conduit.Binary` module. Let's take a stab at implementing our file
copier:

```haskell
import Data.Conduit
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
```

However, GHC doesn't like this:

```
test.hs:5:37:
    No instance for (MonadResource IO)
      arising from a use of `CB.sinkFile'
    Possible fix: add an instance declaration for (MonadResource IO)
    In the second argument of `($$)', namely `CB.sinkFile "output.txt"'
    In the expression:
        CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
    In an equation for `main':
        main = CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
```

Well... *that's* a bit scary looking. What's going on?

When dealing with scarce resource allocation (like file handles), we have two
concerns:

* Make sure the resource is freed as soon as possible.
* Make sure its freed even if an exception is thrown.

`conduit` itself can guarantee the first point, but
&lt;proof-by-assertion&gt;it's impossible for it to handle the second point on
its own.&lt;/proof-by-assertion&gt; In order to deal with such cases, `conduit`
relies on a separate package, `resourcet`, which provides a `ResourceT` monad
transformer. This transformer allows code to register actions which must be
performed, such as closing a file handle.

So what about that `MonadResource` message? A monad stack is an instance of
`MonadResource` if any of the transformers in its stack are a `ResourceT`. So
the simplest instance of `MonadResource` would be `ResourceT IO`. If we create
a helper function, we can write:

```haskell
copier :: ResourceT IO ()
copier = CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
```

In order to unwrap that `ResourceT`, we need the `runResourceT` function. So
our `main` function could be:

```haskell
main :: IO ()
main = runResourceT copier
```

Of course, we can just inline that into a single function, and we get the program:

```haskell
import Data.Conduit
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = runResourceT $ CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
```

Other monads
------------

A number of the functions provided by `conduit` have special `Monad` classes in
the context. For example, text encoding and decoding has a `MonadThrow`
context, to indicate that the action could fail (i.e., a character set doesn't
support the given character, or a byte sequence is not valid for a given
character set). In `zlib-conduit`, the compression and decompression functions
live in `MonadUnsafeIO`, since calling out toe the zlib C library requires
performing some `IO` action.

Any monad stack built on top of `IO` is an instance of `MonadThrow`. In such a
case, any exceptions thrown will be turned into runtime exceptions, as if you
called `throwIO`. If you want to use one of these functions from pure code, you
can use the special `ExceptionT` transformer provided by the `resourcet`
package.

Any stack built on top of `IO` or `ST` is an instance of `MonadUnsafeIO`. This
means you can still compress and decompress data in pure code.

Conduits
========

Now we come to the namesake of our package: the `Conduit`. While a `Source`
produces a stream of data, and a `Sink` consume a stream, a `Conduit`
*transforms* a stream. A simple example of such a transform would be the `map`
function. Let's modify our first program (printing 1 to 10) by adding 1 to each
number before it's printed:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = CL.sourceList [1..10] $= CL.map (+ 1) $$ CL.mapM_ print
```

We've introduced a new operator: `$=`. This is the __left fuse__ operator, and
it fuses a `Source` and a `Conduit` into a new `Source`. We then use the same
old `$$` operator to connect this newly created `Source` with our `Sink`.

You might be wondering: if there's a left fuse, is there also a right fuse? Yes
there is:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

main :: IO ()
main = CL.sourceList [1..10] $$ CL.map (+ 1) =$ CL.mapM_ print
```

`=$` fuses a `Conduit` and a `Sink` into a new `Sink`. So what's the difference
between these two programs? Nothing really. When all you're doing is creating a
simple pipeline to be run, it doesn't make much of a difference whether you
left-fuse or right-fuse. (Conceivably, performance may differ, but I currently
don't have any evidence to show that once approach is consistently faster.)

However, in some circumstances, you'll only have one option available. For
example, let's go back to file reading and writing. The functions provided by
`conduit` only provide byte streams, not textual streams. Suppose we want to
create some helper functions for reading and writing UTF8 files. We would need
something like:

```haskell
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Text (Text)

sourceFileUtf8 :: MonadResource m => FilePath -> Source m Text
sourceFileUtf8 fp = CB.sourceFile fp $= CT.decode CT.utf8

sinkFileUtf8 :: MonadResource m => FilePath -> Sink Text m ()
sinkFileUtf8 fp = CT.encode CT.utf8 =$ CB.sinkFile fp
```

There's also one other form of fusion: __middle fusion__. In this case, you
combine two `Conduit`s together into a new `Conduit`. The operator is `=$=`,
and its usage is just like the other two fusion operators:

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL

plusOneRemoveEvens :: Monad m => Conduit Int m Int
plusOneRemoveEvens = plusOne =$= removeEvens

plusOne :: Monad m => Conduit Int m Int
plusOne = CL.map (+ 1)

removeEvens :: Monad m => Conduit Int m Int
removeEvens = CL.filter odd

main :: IO ()
main = CL.sourceList [1..10] $$ plusOneRemoveEvens =$ CL.mapM_ print
```

Types
=====

So far, we haven't looked at the types themselves very much. Let's address that:

```haskell
type Source m output
type Conduit input m output
type Sink input m result
```

You'll notice four different type parameters: `m` (our monad), `input` (the
type of the stream of input), `output` (the type of the stream of output), and
`result` (the type of the final result generated). Note that `output` and
`result` are *not the same*: a `Sink` produces a single result value, but does
not produce any output stream. On the other hand, but `Source` and `Conduit`
produce output streams, but do not produce a result. And both `Conduit` and
`Sink` consume an input stream, but `Source` does not.

You'll notice that I used `type` above, instead of `data` or `newtype`. The
fact is that all three types are just type synonyms around a single, underlying
datatype: `Pipe`. The full definitions of these types (after shortening
`input`, `output`, and `result` to `i`, `o`, and `r` respectively) are:

```haskell
data Pipe i o m r
type Source m a    = Pipe Void a    m ()
type Sink i m r    = Pipe i    Void m r
type Conduit i m o = Pipe i    o    m ()
```

`Void` is a type provided by the `void` package which has no values, and
therefore can't be created (well, excluding `undefined` that is). So what we're
saying is that a `Source` cannot have any input, a `Sink` cannot have any
output, and a `Source` and `Conduit` produce `()` as a result. (If you're
wondering why we didn't use `()` for the input in `Source` or a number of other
possible things, that's not in the scope of our discussion here. Feel free to
hit up Google or the mailing lists for more information.)

Most high-level usage of `conduit` will never interact with this `Pipe` type.
However, error messages may still refer to `Pipe` occassionally. So it's
important to know that it's there, but most usage should just focus on the
three main types.

# TODO

* connect-and-resume
* create your own source/sink/conduit
* semantics: right-biased
* resource finalization rules
