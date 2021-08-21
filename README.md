# AUTOCOMF - Automatic Configuration from Comments

**AUTOCOMF** is a programming language that presents a configuration UI
based on annotations in a source file.  The configuration specified
by the user is then incorporated by directly modifying the source
file.

**AUTOCOMF** can be added seamlessly to *any* programming language or
configuration language, since it is written exclusively within the
comment syntax of the host language.

**AUTOCOMF** has incorporates several attractive qualities:

* Self-modifying code.

* Semantically meaningful comments.

* ALL CAPS KEYWORDS.

* A human-friendly, intuitive, unclearly specified syntax.

* Named in honour of the universally loved
  [autoconf](https://www.gnu.org/software/autoconf/) system.

**AUTOCOMF** is guaranteed free of defects, but user error is always a
possibility.  In the best Unix tradition, **AUTOCONF** will
irrevocably change the input file, without any backup!  Make sure you
understand [the completely precise and unambiguous
specification](https://github.com/athas/autocomf/blob/main/src/Main.hs)
before running it on files you care about!

**AUTOCOMF** was developed as a contribution to [Langjam
#1](https://github.com/langjam/jam0001).

## Usage

You'll need a Haskell setup (install from your package manager [or use
ghcup](https://www.haskell.org/ghcup/)).  Then build with:

```
$ cabal update
$ cabal build
```

And run an example program with:

```
$ cabal run autocomf examples/bashrc
```
