# Tools

Obviously the most important tool when writing Haskell will be your compiler
(most likely GHC). However, it's not going to be the only one. These chapter
will cover some of the other tools available to be a productive Haskell
developer.

__FIXME__: Include some information on configuring common text editors (Vim,
Emacs) or using an IDE (EclipseFP, Leksah). Also cover version control
(Git/Darcs/Mercurial?) and mention code sharing sites.

## Cabal

Cabal refers to a few different things:

* It's a file format for giving basic metadata about a package (name, version,
  description, etc), state what should be built, dependencies, and tests.

* It's also the name of a package containing a bunch of code for dealing with
  these files.

* Finally, it's the name of a command line tool. Perhaps confusingly, this tool
  is provided by the `cabal-install` package, not `Cabal` itself. When we say
  `cabal` here, we're referring to the command line tool.

`cabal` is the defacto standard for installing Haskell packages, and building
your own code. Another defacto standard is Hackage, the default package
repository. This is where you'll be getting the majority of your code from, and
perhaps also where you'll be releasing code to eventually.

Let's run through some of the basic commands:

* `cabal update` downloads a new list of packages from Hackage. You should do
  this fairly regularly.

* `cabal install foo` downloads and install the `foo` package from Hackage.

* `cabal init` asks you some questions, and then creates a new .cabal file for
  you.

* `cabal install` builds and installs the package in the current folder, along
  with all of its dependencies.

* `cabal configure` will configure the package in the current folder. It can
  fail if dependencies are not installed. In this case, you can use
  `cabal install --only-dependencies`.

* `cabal build` will build a configured package without installing it.

### Recommendation: Always use a cabal file

__Note__: The following dramatization is based on a true story.

Here's a fairly common situation. Someone sits down and decides he/she needs to
write some tool. "It'll just be a 10-line program, and I'll never need it
again." Five hours later, he/she is looking at 300 lines of code and depends on
15 packages. Our developer compiles this tool with `ghc --make tool.hs`, and it
turns out to be incredibly useful. Soon, everyone in the company is using it,
and within three months no one can get by without it.

One day, someone finds a bug, or states that a new feature is absolutely
necessary. So our developer dusts off the source code, runs `ghc --make
tool.hs`, and gets a bunch of error message. "I don't get it," he/she says, "it
compiled perfectly three months ago." What happened? Three months ago, the
`foo` package was at version 0.4, and provided the `Bar` type. Now version 0.5
is out, it doesn't provide that type, and a bunch of the other dependencies are
no longer compatible.

Now our developer is truly in a cunundrum. There are two choices:

* Try to replicate the build environment from three months ago. This will be
  incredibly difficult.

* Fix the code to work with the newest versions of all packages in question.

Another problems comes when some new person needs to compile the tool, and
doesn't even know which package has the `Foo.Bar.Baz` module. So for all 30
import statements, this new developer will be stuck searching Hackage to
determine which package should be used.

Moral of the story: even for one-time scripts, write a cabal file. Your future
self will thank you.

## cabal-dev

`cabal` does have some warts. The cabal team are actively working on these, and
the system is improving. In the interim, there are some additional tools people
use as workarounds. Probably the most popular is `cabal-dev`.

__Note__: A newer option is virthualenv. This chapter focuses on `cabal-dev`
instead both because the author is more familiar with it, and because it works
on more OSes.

One of the big issues with `cabal` is *sandboxing*. Suppose there is a package
`foo` with versions 2 and 3. There's also a package `bar` which requires either
version of `foo`.

I'm working on two projects, project A and project B. Project A requires `foo`
version 2, *and* `bar`. Project B requires `foo` version 3, and `bar` as well.
Guess what? Installing project A will break B, and vice versa. That's because
each one will overwrite the previous version of `bar`.

`cabal-dev` neatly solves this problem with *sandboxing*. Instead of having a
single package database per user, and installing all packages there, each
project gets its own database. Usage is very simple: just replace `cabal` with
`cabal-dev`. Everything should work as expected.

The only real downside of this approach is longer compile times. For example,
if you have two different sites based on Yesod, and you use `cabal-dev` for
both sites, you will have to compile all of the dependencies from scratch when
you start each project. However, most users agree that this is a price worth
paying to avoid broken package databases.

## cabal-src

Suppose you're working on a large project that spans many packages. Yesod is a
big example of this: there are about 12 different packages in the WAI repo, 8
in the Persistent repo, 6 in the Shakespeare repo, and 15 in the Yesod repo.
Now imagine you want to test a change to the `text` package, which virtually
every one of the other packages depends on. Typically, you would have to go
into each of those individual folders, rebuild, and reinstall. The reason is
that when `cabal` installs a package, it only installs the produced binary, and
has no way to rebuild the package if a dependency changes.

`cabal-src` was created to solve this problem. Instead of installing a package
with `cabal install`, you use `cabal-src-install`. After the package has been
built and installed, a source distribution is built and installed to a local
database. `cabal` now treats that package the same as it would treat a package
available on Hackage, and will rebuild and install it automatically.

__FIXME__: Other tools like cabal-meta
