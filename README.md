# Gravatar

Generate Gravatar images URLs.

## Installation

```
% cabal install gravatar
```

## Usage

```hs
gravatar def "pbrisbin@gmail.com"
-- => https://www.gravatar.com/avatar/2be502055b6c21ff470730beead2a998
```

For more information, see the [haddocks][].

[docs]: http://hackage.haskell.org/package/gravatar/docs/Network-Gravatar.html

## Developing & Tests

```
% cabal install --dependencies-only --enable-tests
% cabal test
```

## History

This is a clean-room implementation of [this][] existing package (I had no idea
it existed when I wrote this).

[this]: http://hackage.haskell.org/package/gravatar-0.3

There's only a few small differences:

1. I have only one function and a required "options" argument which supports
   everything the Gravatar API offers.
2. My `gravatar` operates on a `Text` value

The older package hasn't been updated since 2008 and I'm unable to reach Don on
the matter. Hopefully it's OK that I take over the module name.
