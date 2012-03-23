# Example: File Hash Lookup

__Learning Objectives__:

* Streaming data via `conduit`
* `crypto-api` and `crypto-conduit` for hashing
* Understanding the pitfalls of lazy I/O
* Better file path management via `system-filepath`
* More efficient lookups with `unordered-containers`

__Use Case__:

* Work on a folder containing a large number of files.
* User provides the MD5 hash of one of the files.
* Application reports path to matching file, if present.

_Note_: A simple optimization for this program would be to cache information
between runs. We will not be doing so here, it is left as an exercise to the
reader.

## Simple approach: lazy I/O

Let's start off with a basic approach. We'll use the standard file access
functions from `directory`, `pureMD5` for hashing, and will read file contents
with lazy I/O. We'll start off with an import list:

```haskell
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import Data.Word (Word8)
import Numeric (showHex)

import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import System.Environment (getArgs, getProgName)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import Data.Digest.Pure.MD5 (md5)
import Data.Serialize (encode)
```

We want to represent the MD5 sum as a sequence of hexadecmal characters. Let's
define a simple helper function to convert a `ByteString` to a hex
representation. Note that this implementation is not particularly efficient.
Writing a more efficient version in terms of `unfoldrN` is left as an exercise
to the reader.

```haskell
toHex :: S.ByteString -> S.ByteString
toHex =
    S.concatMap word8ToHex
  where
    word8ToHex :: Word8 -> S.ByteString
    word8ToHex w = S8.pack $ pad $ showHex w []

    -- We know that the input will always be 1 or 2 characters long.
    pad :: String -> String
    pad [x] = ['0', x]
    pad s   = s
```

Next, we'll want to have a function that takes the entire contents of a file as
a lazy `ByteString` and returns its hex representation. This is actually very
simple:

```haskell
hash :: L.ByteString -> S.ByteString
hash = toHex . encode . md5
```

The one trick comes from understanding the need for `encode`. The `md5`
function returns a `MD5Digest` value, which we must convert to a `ByteString`
via the `cereal` package.

Now let's get into the meat of this program. We want to take a `FilePath` to a folder, and get a list of pairs mapping hash to `FilePath`.

```haskell
buildMap :: FilePath -> IO [(S.ByteString, FilePath)]
buildMap dir = do
    fps <- getDirectoryContents dir
    catMaybes <$> mapM getPair fps
  where
    getPair :: FilePath -- ^ filename without directory!
            -> IO (Maybe (S.ByteString, FilePath))
    getPair name = do
        exists <- doesFileExist fp
        if exists
            then do
                lbs <- L.readFile fp
                return $ Just (hash lbs, fp)
            else return Nothing
      where
        fp = dir </> name
```

`getDirectoryContents` returns a list of filenames contained in a specific
folder. Note that this is a filename *without* the folder. You need to manually
add the folder name to the beginning. We use the `filepath` package's `</>`
operator to do so.

Also, `getDirectoryContents` returns both files and folders, so we need to
explicitly check (via `doesFileExist`) if the path we are looking at is a file.
If it is, we read the contents in lazily, hash them, and return the pair of
hash and filepath. We use `catMaybes` to get rid of any `Nothing` values
returned.

Finally, we have our `main` function:

```haskell
main :: IO ()
main = do
    args <- getArgs
    (folder, needle) <-
        case args of
            [a, b] -> return (a, b)
            _ -> do
                pn <- getProgName
                error $ concat
                    [ "Usage: "
                    , pn
                    , " <folder> <needle>"
                    ]
    md5Map <- buildMap folder
    case lookup (S8.pack needle) md5Map of
        Nothing -> putStrLn "No match found"
        Just fp -> putStrLn $ "Match found: " ++ fp
```

This program seems to work properly:

    $ md5sum README
    d41d8cd98f00b204e9800998ecf8427e  README
    $ runghc simple.hs . d41d8cd98f00b204e9800998ecf8427e
    Match found: ./README

## Problem 1: FilePath handling

There are in fact four problems with how we've dealt with `FilePath`s:

1. It's tedious and error-prone to have to prepend the folder name to the
   results of `getDirectoryContents`.

2. Having to check whether a `FilePath` is a file or folder is, again, tedious
   and error-prone.

3. We are likely not handling character encodings of the paths properly.
   `FilePath` is defined as a type synonym for `[Char]`, but in fact does not
   properly handle encodings on all systems.

4. We're ignoring the contents of subfolders. We haven't actually specified
   whether files in subfolders should be inspected or not, so this is not
   actually a bug, but a feature enhancement.

We'll start with issues 1 and 3. These can be addressed by relying on a
separate library for better filepath handling, `system-filepath`, and its
associated `system-fileio`. The former defines an abstract datatype for
representing paths, along with a number of utility functions for manipulating
them. The latter exposes functions for interacting with the filesystem.

The simplest way to switch over to this library is to add two import
statements:

```haskell
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString)
```

All we're doing is replacing the standard type synonym definition of `FilePath`
with `system-filepath`'s abstract definition, as well as importing functions to
convert to and from normal strings.
`filepath` with `system-filepath`'s version.

After that, start try to compile and start changing code. We have a few simple
fixes in `main`: `md5Map <- buildMap folder` becomes
`md5Map <- buildMap $ decodeString folder` and
`++ fp` becomes `++ encodeString fp`.

For the next two changes, we'll need to replace our imports from `System.Directory` with:

```haskell
import Filesystem (listDirectory, isFile)
```

In place of `getDirectoryContents`, we'll use `listDirectory`. However, this
new function will return a full file path, not just the filename. We also
replace `doesFileExist` with `isFile`. In other words, the beginning of
`getPair` is now:

```haskell
getPair fp = do
    exists <- isFile fp
```

Much simpler!

We'll come back to problems 2 and 4 later, after we introduce `conduit`s.

## Problem 2: Laziness

How much memory does our program consume? Seemingly, not very much. We never
read the entire body of the files into memory, we only read in one chunk at a
time. So most likely, we read in a chunk, update the state of our MD5 digest,
and then discard that chunk entirely. Lazy I/O to the rescue!

While this may be true, we actually have an entirely different form of resource
exhaustion on our hands: file descriptors. Do you know precisely when the file
handles will be open. It's not obvious if this happens when we first call
`readFile`, or when the hash value is first evaluated. And how about when the
file handles are closed? It's completely non-deterministic.

If you have 50 files in your folder, no big deal. But suppose you have 5000, or
200,000. You'll quickly run out of file descriptors! This isn't just an
academic concern; this kind of question has come up multiple times on the
Haskell cafe, and was a bug at one point in `yesod-static`.

One possible solution would be to try adding `seq` in a few places and force
evaluation early. But even this isn't completely deterministic. Instead, let's
tackle this problem by using a library designed to avoid lazy I/O: `conduit`.

In `conduit`, we have three main datatypes: a `Source` is a producer of data, a
`Sink` is a consumer of data, and a `Conduit` is a transformer of data. For our
example, we'll need the `sourceFile` function to produce a stream of bytes from
a file and the `sinkHash` function to consume a stream of bytes into a digest.
We won't be using a `Conduit` here, but a possible usage would be to
automatically decompress any files with a .gz file extension. Adding this
enhancement would be an excellent exercise.

Note: We're actually taking the "long way around" in implementing this, since
the `crypto-conduit` package conveniently provides a `sinkFile` function
already. We're doing this all manually to demonstrate how to use `conduit`.

Let's add our import statements:

```haskell
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Filesystem (sourceFile)
import Crypto.Conduit (sinkHash)

import Data.Digest.Pure.MD5 (MD5Digest)
```

We've already explained `sourceFile` and `sinkHash`. One note about the former:
note that it is imported from the `Data.Conduit.Filesystem` module. This module
provides functions that work with `system-filepath`'s `FilePath` type by
default. This means we don't have to do any encoding or decoding of `String`
values.

The `$$` operator is known as the __connect operator__. It pulls data from a
`Source` and pushes it to a `Sink`. This is a completely strict, deterministic
action. We are guaranteed that the file handle will be opened immediately, that
data will be pulled one chunk at a time, sent to the sink, and then discarded.
As soon as the last chunk of data is pulled, or when the sink completes
processing early, the source is closed.

The one problem with that statement is exceptions. If we had a sink that threw
an exception halfway through reading a file, we want to be certain that the
file handle is still closed. For this, we have `runResourceT`. Any `conduit`
function which allocates scarce resources must live in a `MonadResource`. When
the file handle is first opened, a release action is registered to close the
file handle. If processing terminates normally, the file handle is closed
immediately. If an exception is thrown, `runResourceT` will catch the
exception, perform any cleanup actions, and then rethrow the exception. Either
way, you can't leak a resource.

That was a pretty long description, but it all boils down to one line of code:

```haskell
digest <- runResourceT $ sourceFile fp $$ sinkHash
```

All we're doing is reading from the file, connecting to the hash-producing
function, and pulling out the digest.

Next, we need to turn this `digest` value into a hex-encoded `ByteString`. Like
previously, we'll need to use `cereal`'s `encode` function. However, `sinkHash`
can work with many different kinds of hash algorithms (e.g., skein, SHA256). So
we need to explicitly tell GHC which type of digest we want. We do this by
giving an explicit signature to `digest`:

```haskell
let hash = toHex . encode $ (digest :: MD5Digest)
```

The `cryptohash` package provides a large number of hashes. Since we're just
sticking with MD5, this example still uses the `pureMD5` package.

## Dealing with those subfolders

Let's see another example of `conduit`. The `Data.Conduit.Filesystem` module
provides a function `traverse`, which gives a `Source` of all files in a
folder, or any of its subfolders. This can help us deal with points 2 and 4
from problem #1: it will only provide files, not folders, and will
automatically traverse subfolders. We'll need to update our imports a bit more:

```haskell
import Data.Conduit (($$), (=$), runResourceT)
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem (sourceFile, traverse)
```

The `=$` operator is called __right fuse__. It combines a `Conduit` and a
`Sink` together into a `Sink`. The `Data.Conduit.List` module provides a number
of familiar functions for working with conduit, such as `mapM` and `fold`.
Let's see how we put this together:

```haskell
buildMap :: FilePath -> IO [(S.ByteString, FilePath)]
buildMap dir =
       traverse False dir
    $$ CL.mapM getPair
    =$ CL.consume
  where
    getPair :: FilePath -> IO (S.ByteString, FilePath)
    getPair fp = do
        -- Now we know that fp is a file, not a folder.
        -- No need to check it.
        digest <- runResourceT $ sourceFile fp $$ sinkHash
        let hash = toHex . encode $ (digest :: MD5Digest)
        return (hash, fp)
```

The first argument to `traverse` indicates whether we should follow symbolic
links. We've elected not to. Notice how we connect this two the `CL.mapM
getPair`, and fuse that with `CL.consume`. What we're really doing is:

* Fuse `CL.mapM getPair` and `CL.consume` into a new `Sink`.
* Connecting that new `Sink` with the `traverse` `Source`.

In this way, it's easy to build up pipelines of operations.

`mapM_` does what you would expect: it transforms each element in a stream
using some monadic function. `consume` will read in a stream of values and
store them as a list. By fusing these two actions together, we're creating a
`Sink` that will convert a stream of `FilePath`s into a list of pairs of hashes
and `FilePath`s.

We're also able to completely skip the `isFile` check at this point. In other
words: mission accomplished.

## Problem 3: Inefficient lookup

One last annoyance: that `lookup` we're performing in `main` is an O(n)
operation. Since we're only ever doing a single lookup, we could restructure
our program to simply terminate as soon as it finds a matching hash value. And
in fact, that would be the most efficient thing we could do, given our current
constraints.

But suppose we want to change our program to allow a user to enter multiple
hash values to be lookup up, or we want to create a server that will respond.
We'll want to cache all of the hash values when our program starts, and
continue using that throughout the duration of the application. To do this,
we'll use the `unordered-containers` package's `HashMap`.

First we'll need to import the module in question:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
```

Modifying the main function is simple, just replace `lookup` with
`HMap.lookup`. The real work comes from `buildMap`, but even that's not too
bad:

```haskell
buildMap :: FilePath -> IO (HashMap S.ByteString FilePath)
buildMap dir =
       traverse False dir
    $$ CL.mapM getPair
    =$ CL.fold HMap.union HMap.empty
  where
    getPair :: FilePath -> IO (HashMap S.ByteString FilePath)
    getPair fp = do
        -- Now we know that fp is a file, not a folder.
        -- No need to check it.
        digest <- runResourceT $ sourceFile fp $$ sinkHash
        let hash = toHex . encode $ (digest :: MD5Digest)
        return $ HMap.singleton hash fp
```

Instead of returning a tuple, `getPair` now returns a `HashMap`. And instead of
using `CL.consume`, we use `CL.fold` to join together each successive
`HashMap`.

### Possibly misguided optimization

If we wanted to optimize this a bit more, we could skip the creation of the
intermediate `HashMap`s and avoid the intermediate `Conduit`, by rewriting our
code as:

```haskell
buildMap :: FilePath -> IO (HashMap S.ByteString FilePath)
buildMap dir =
    traverse False dir $$ CL.foldM addFP HMap.empty
  where
    addFP :: HashMap S.ByteString FilePath
          -> FilePath
          -> IO (HashMap S.ByteString FilePath)
    addFP hmap fp = do
        digest <- runResourceT $ sourceFile fp $$ sinkHash
        let hash = toHex . encode $ (digest :: MD5Digest)
        return $ HMap.insert hash fp hmap
```

This is called possibly misguided since there's no actual evidence that this
will speed up the code. As much as our gut may say "look, there are less lines
of code," without profiling it, we can't be certain. At this point, it's a
matter of style whether you prefer the previous version of `buildMap` or this
one.

The former is nicer since it clearly separates between two separate actions
(turning a `FilePath` into the hash-pair, and combining multiple `HashMap`)
whereas this in some ways makes it clearer what is going on (I'm inserting into
an existing map). Which approach you take is entirely your decision.

## Final source code

```haskell
import Prelude hiding (FilePath)
import Data.Word (Word8)
import Numeric (showHex)

import System.Environment (getArgs, getProgName)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString)

import Data.Conduit (($$), runResourceT)
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem (sourceFile, traverse)
import Crypto.Conduit (sinkHash)

import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Serialize (encode)

main :: IO ()
main = do
    args <- getArgs
    (folder, needle) <-
        case args of
            [a, b] -> return (a, b)
            _ -> do
                pn <- getProgName
                error $ concat
                    [ "Usage: "
                    , pn
                    , " <folder> <needle>"
                    ]
    md5Map <- buildMap $ decodeString folder
    case HMap.lookup (S8.pack needle) md5Map of
        Nothing -> putStrLn "No match found"
        Just fp -> putStrLn $ "Match found: " ++ encodeString fp

buildMap :: FilePath -> IO (HashMap S.ByteString FilePath)
buildMap dir =
    traverse False dir $$ CL.foldM addFP HMap.empty
  where
    addFP :: HashMap S.ByteString FilePath
          -> FilePath
          -> IO (HashMap S.ByteString FilePath)
    addFP hmap fp = do
        digest <- runResourceT $ sourceFile fp $$ sinkHash
        let hash = toHex . encode $ (digest :: MD5Digest)
        return $ HMap.insert hash fp hmap

-- Overall, this function is pretty inefficient. Writing an optimized version
-- in terms of unfoldR is left as an exercise to the reader.
toHex :: S.ByteString -> S.ByteString
toHex =
    S.concatMap word8ToHex
  where
    word8ToHex :: Word8 -> S.ByteString
    word8ToHex w = S8.pack $ pad $ showHex w []

    -- We know that the input will always be 1 or 2 characters long.
    pad :: String -> String
    pad [x] = ['0', x]
    pad s   = s
```