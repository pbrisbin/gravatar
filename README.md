# Gravatar

~~~ { .haskell }
import Network.Gravatar
~~~

Lookup gravatar image urls based on an email address.

~~~ { .haskell }
url :: String
url = gravatar def "pbrisbin@gmail.com"
-- ==> https://secure.gravatar.com/avatar/2be502055b6c21ff470730beead2a998
~~~

This is a clean-room implementation of [this][] existing package (I had 
no idea it existed when I wrote this).

[this]: http://hackage.haskell.org/package/gravatar-0.3

There's only a few small differences:

1. I have only one function and a required "options" argument which 
   supports everything the Gravatar API offers.

2. My `gravatar` operates on a `Text` value

