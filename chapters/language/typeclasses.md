# Type Class Extensions

Let's consider the `OverloadedStrings` language extension. This replaces any
occurence of a string literal in your code with a call to `fromString`. In
other words:

```haskell
foo "bar"
```

becomes

```haskell
foo (fromString "bar")
```

`fromString` is a member of the `IsString` typeclass, which is defined as:

```haskell
class IsString a where
    fromString :: String -> a
```

## Multi Param Type Classes

It can be very convenient to have a function that converts from a `String` to
other types. But this isn't just convenient for `String`. Would it be nice to
be able to convert a list of bytes (i.e., `Word8`s) to a `ByteString`? Or how
about a `Vector` of `Char`s to a `Text`?

We could of course define a number of different type classes:

```haskell
class IsCharVector a where
    fromCharVector :: Vector Char -> a
class IsByteList a where
    fromByteList :: [Word8] -> a
```

But that will become tedious. Instead, we can use a language extension called
`MultiParamTypeClasses` (MPTC). With this extension, type classes can have more than
one type parameter. We can now create a generate purpose conversion class:

```haskell
class Convert a b where
    convert :: a -> b

instance Convert [Char] T.Text where
    convert = T.pack
instance Convert [Word8] S.ByteString where
    convert = S.pack
```

## Functional Dependencies

While working with the `text` and `bytestring` libraries, we notice that both
packages provide `unpack` functions, which convert the value to a list. We can
use MPTCs to create a general `unpack` function:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Word
import qualified Data.ByteString as S
import qualified Data.Text as T

class Unpack a b where
    unpack :: a -> [b]
instance Unpack [a] a where
    unpack = id
instance Unpack S.ByteString Word8 where
    unpack = S.unpack
instance Unpack T.Text Char where
    unpack = T.unpack

main = do
    let foo = T.pack "foo"
    print $ length $ unpack foo
```

Unfortunately, when we try to compile this, we get a confusing error message:

    test.hs:15:22:
        No instance for (Unpack T.Text a0)
          arising from a use of `unpack'
        Possible fix: add an instance declaration for (Unpack T.Text a0)
        In the second argument of `($)', namely `unpack foo'
        In the second argument of `($)', namely `length $ unpack foo'
        In the expression: print $ length $ unpack foo

"What do you mean?" you shout out GHC. "Just look! Of course I created an
instance of Unpack for T.Text!" The problem is that GHC doesn't know that you
expect `unpack` to create a list of `Char`s. All you've specified here is that
GHC should create a list of *something*, and that you want to count how many
*something*s there are.

This might be a bit more clear if we separate out the code as its own function.
If we write:

```haskell
count :: Unpack a b => a -> Int
count = length . unpack
```

GHC gives the slightly more helpful:

    test.hs:19:18:
        Ambiguous type variable `b' in the constraint:
          (Unpack a b) arising from a use of `unpack'
        Probable fix: add a type signature that fixes these type variable(s)
        In the second argument of `(.)', namely `unpack'
        In the expression: length . unpack
        In an equation for `count': count = length . unpack

What this says is, "You told me to pick the right instance based on `b`, but
then you never actually *use* `b`. How can I decide which is the right
instance?" You may know that for each `a` there will only ever be one instance,
but GHC doesn't. You need to tell it so explicitly.

The way to do that is with *functional dependencies*, aka fundeps. The code change
is fairly minor. Add the language pragma `{-# LANGUAGE FunctionalDependencies #-}`
to the top of your file, and then redefined the `Unpack` class as:

```haskell
class Unpack a b | a -> b where
    unpack :: a -> [b]
```

What that says is "Unpack takes parameters `a` and `b`, and `a` *determines*
what `b` is." You can no longer define two different instances for `T.Text`,
and when GHC sees `unpack foo`, it knows you want the result to be a list of
`Char`s.

__FIXME__: Demonstrate the reverse dependency as well, e.g. `a -> b, b -> a`.
It plays well with the associated types example.

## Type families

This is nice; now our definition of `count` works. But it's a bit strange to
have that `b` floating around, isn't it? There's an alternate approach to this
problem, known as *type families* or associated types. You can reimplement
`Unpack` like so:

```haskell
{-# LANGUAGE TypeFamilies #-}
import Data.Word
import qualified Data.ByteString as S
import qualified Data.Text as T

class Unpack a where
    type Elem a
    unpack :: a -> [Elem a]
instance Unpack [a] where
    type Elem [a] = a
    unpack = id
instance Unpack S.ByteString where
    type Elem S.ByteString = Word8
    unpack = S.unpack
instance Unpack T.Text where
    type Elem T.Text = Char
    unpack = T.unpack

count :: Unpack a => a -> Int
count = length . unpack

main = do
    let foo = T.pack "foo"
    print $ count foo
```

This code is a bit more verbose, but it does get rid of that dangling `b`
parameter. It also let's us refer to the contained element specifically.

__FIXME__ need more input: Describe why to use one or the other. Are there any use cases today for which FunDeps work and TFs don't?

__FIXME__ include an example of using an associated data family.

## Equality constraints

Suppose we now want to compare two instances of `Unpack`, extending the
type family example. The two instances of `Unpack` need to unpack to the
same data type. This can be expressed with an equality constraint:

```haskell
unpackEquals :: (Unpack a, Unpack b, Eq (Elem a), Eq (Elem b), Elem a ~ Elem b)
                -> a -> b -> Bool
unpackEquals x y = unpack x == unpack y
```
