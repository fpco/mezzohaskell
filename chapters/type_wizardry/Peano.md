# Peano Numbers

Peano numbers are a simple representation of natural numbers (that is; whole 
numbers from 0 onwards). They are widely used, especially at the type level, as 
they are easier to reason about than their digital counterparts. Here is a 
Haskell definition.

```haskell
data Peano = Zero | Successor Peano
```

The number three would be represented as:

```haskell
(Successor (Successor (Successor Zero)))
```

In practice you are far more likely to come across Peano numbers at the type 
level:

```haskell
data Zero = Zero
data Successor a = Successor
```

This allows us to do some really interesting things. For example, we can create 
a list type where we know it's length at compile time:

```haskell
data Vec a l where
  Nil :: Vec a Zero
  Cons :: a -> Vec a l -> Vec a (Successor l)
```

If you haven't seen a data declaration like this before, that's because it's a 
GADT (Generalized Algebraic Data Type). We can also define some type level 
'functions' using type families:

```haskell
type family Plus a b
type instance (Plus Zero b) = b
type instance (Plus (Successor a) b) = (Successor (Plus a b))

type family Mult a b
type instance (Mult Zero b) = Zero
type instance (Mult (Successor a) b) = Plus b (Mult a b)
```

This is where the fun really begins, definitions like this become possible:

```haskell
appendV :: Vec x a -> Vec x b -> Vec x (Plus a b)
appendV (Cons x xs) ys = (Cons x (appendV xs ys))
appendV Nil ys = ys
```

It's nice to be able to do this, but you may be asking "what does it buy us?". 
The next function will demonstrate it's real use. If you pass two differently 
sized lists into the `zip` function in prelude, the resultant list will be the 
same size as the smaller one, but knowing the length at compile time allows us 
to prevent any such truncation:

```haskell
zipV :: Vec x a -> Vec y a -> Vec (x, y) a
zipV Nil Nil = Nil
zipV (Cons x xs) (Cons y ys) = Cons (x, y) (zipV xs ys)
```

For completeness, here's unzip:

```haskell

unzipV :: Vec (x, y) a -> (Vec x a, Vec y a)
unzipV Nil = (Nil, Nil)
unzipV (Cons (a, b) x) =
  let (as, bs) = unzipV x in
    (Cons a as, Cons b bs)
```

Similiarly, `head` and `tail` are partial functions in prelude, but now they 
don't have to be. Knowing that (Successor a) can be any number from one 
onwards:

```haskell
headV :: Vec x (Successor a) -> x
headV (Cons item _) = item

tailV :: Vec x (Successor a) -> Vec x a
tailV (Cons _ rest) = rest
```

We're not as limited as you might imagine, it's even possible to define 
functions like `concat`:

```haskell
concatV :: Vec (Vec a y) x -> Vec a (Mult x y)
concatV Nil = Nil
concatV (Cons x xs) = appendV x (concatV xs)
```

And of course, what list library would be complete without `fold` and `map`:

```haskell
foldV :: (a -> b -> b) -> b -> Vec a l -> b
foldV _ init Nil = init
foldV f init (Cons x xs) = f x (foldV f init xs)

mapV :: (a -> b) -> Vec a l -> Vec b l
mapV _ Nil = Nil
mapV f (Cons x xs) = Cons (f x) (mapV f xs)
```

Note: I would have defined `mapV` in terms of `foldV`, but this was not 
possible without complicating `foldV`. Can you see why?

This isn't a panacea though, Haskell is not a dependently typed languages, so 
it's very easy to end up with irrecoverable type errors like:

```
Couldn't match type `l' with `Plus l Zero'
```

Another problem is demonstrated by the `filter` function. What's it's type? 
It's very easy to get bitten, but being able to create descriptive types like 
this is a hugely useful skill to an accomplished Haskeller when used 
appropriately.
