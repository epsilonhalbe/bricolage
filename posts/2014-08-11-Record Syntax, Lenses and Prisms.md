---
title: Record Syntax, Lenses and Prisms: Part 2 - Lenses
description: A short introduction to lenses
tags: haskell, eng
---

Lenses
======

Lenses are a quite interesting idea first mentioned by Twan van Laarhoven and
have lived through a few implementations until the `lens`-library by Ed Kmett
has proven to be the stable solution for now. It has a *batteries included*
approach and provides many operators and a template haskell convention to
generate lenses for your own algebraic data types.

The convention is to put an `_` at the beginning of the record names in the
record syntax definition. And then use the magic of template haskell to generate
the corresponding lenses a.k.a. functional getters and setters
with `makeLenses ''MyADT`.

Going back to the old examples one could rewrite it as follows.

For one we need the template haskell language pragma to make the magic work.

~~~haskell

> {-# LANGUAGE TemplateHaskell #-}

~~~

And we also need the lens library to be installed for which I recommend using
`cabal`, the interface to the haskell packaging system.

~~~shell
~ $ cabal update
~ $ cabal install lens
... this may take some time so do something healthy like eat an apple or stretching until you get:
Installed lens-4.3.3 (the current lens version as of 11th of August 2014)
~~~

Next step is to do the import of the lens package and add a whole bunch of
underscores ...

<!--

> module Pirates where
> import Data.Char (toLower, toUpper)
> import Data.List (intercalate)

-->

~~~haskell

> import Control.Lens

> data Human = Attributes { _name :: String, _body :: Body, _age :: Int} deriving (Show)
> data Body = Body { _hat :: Maybe Hat
>                  , _beard :: Maybe Beard
>                  , _torso :: Torso
>                  , _accessories :: [Accessories]}

~~~
<!--

> data Hat = Tricorne | WideBrimmedHat | Bandana deriving (Show)
> data Torso = Naked | Vest | ShabbyShirt deriving (Show)
> data Accessories = Parrot | Monkey | PegLeg | EyePatch | EarRing | Hook deriving (Show)

> data Beard = Beard Colour BeardType
> instance Show Beard where show (Beard c t) = "an exquisite "++ map toLower (show c)++" "++show t
> data Colour = Black | Red | Blond | White | Brown deriving (Show)
> data BeardType = Moustache | Ladybeard | Goatee | FullBeard deriving (Show)

> instance Show Body where
>   show b = "\t Hat: "  ++show (_hat b)++"\n"
>          ++"\t Beard: "++show (_beard b)++"\n"
>          ++"\t Torso: "++show (_torso b)++"\n"
>          ++"\t Accessories: "++ (intercalate ", " $ map show (_accessories b))

-->

~~~haskell

> data Pirate = Captain   { _attributes :: Human, _ship :: String}
>             | FirstMate { _attributes :: Human, _shanty :: String}
>             | Marauder  { _attributes :: Human, _hometown :: String}

~~~

... and of course create the lenses. (In the background template haskell now
creates functions attributes, ship, shanty and so on.)

~~~haskell

> makeLenses ''Human
> makeLenses ''Body
> makeLenses ''Pirate

~~~

Now would be a great moment to talk about the types of Lens and the famous
Lens-laws, but I'd rather have some use of them before I bore you to death.

So let us have a look at the instance declaration for `Show`

~~~haskell

> instance Show Pirate where
>   show (Captain a s)   =  "The infamous Captain "++ a^.name++" of the "++ s++"\n"
>                        ++ show (a^.body)++"\n"
>                        ++ "\t Age: "++show (a^.age)

~~~

<!--

>   show (FirstMate a s) =  "Mate "++a^.name++" sings "++s
>                        ++ show (a^.body)++"\n"
>                        ++ "\t Age: "++show (a^.age)
>   show (Marauder a h) =   "Fearsome Pirate "++a^.name++" from "++h++"\n"
>                        ++ show (a^.body)++"\n"
>                        ++ "\t Age: "++show (a^.age)

-->

I wouldn't call that an improvement but the average object oriented programmer
might. So what is this `^.` operator, it is an alias for the `view`-function
that can focus on the parts of a lens.

The type signature for `(^.)` is a bit complicated, but if we combine it with
the generated functions we see

~~~haskell

GHCi> :t (^.attributes)
(^.attributes) :: Pirate -> Human
GHCi> :t (^.attributes.name)
(^.attributes.name) :: Pirate -> String

~~~

But where the combined type signature of `(^.)` and `attributes` is simple their
own type signature is - let's just call it *not* simple.

~~~haskell

GHCi> :t (^.)
(^.) :: s -> Getting a s a -> a
GHCi> :t attributes
attributes :: Functor f => (Human -> f Human) -> Pirate -> f Pirate

~~~

As we saw we have something called `Getting` so there should also be some
setting stuff and indeed we have a `set`-function and the infix alias `(.~)`
which is more useful with the `&`-operator which is just reverse function
application: `x & f = f x`.

<!--

> cpt = Captain ( Attributes  "Blackbeard"
>                            ( Body ( Just Tricorne)
>                                   ( Just (Beard Black FullBeard))
>                                     Vest
>                                     [Parrot, PegLeg]
>                                   )
>                             42)
>                 "SS Sea Serpent"

> mt1 = FirstMate { _attributes = Attributes { _name = "Redbeard"
>                                            , _body = Body { _hat   = Just WideBrimmedHat
>                                                           , _beard = Nothing
>                                                           , _torso = Naked
>                                                           , _accessories = [EarRing, Monkey]
>                                                           }
>                                            , _age = 30
>                                            }
>                 , _shanty = "What shall we do with the drunken sailor"
>                 }

> mrd = Marauder { _attributes = Attributes { _name = "Neckbeard"
>                                           , _body = Body { _hat   = Just Bandana
>                                                          , _beard = Just (Beard Brown Goatee)
>                                                          , _torso = ShabbyShirt
>                                                          , _accessories = [EyePatch]
>                                                          }
>                                           , _age = 20
>                                           }
>                , _hometown = "Hipsterhausen"
>                }

-->

A few examples should provide a bit more insight...

~~~haskell

> hel = cpt & attributes.name.~ "Hellscream"

GHCi > hel
The infamous Captain Hellscream of the SS Sea Serpent
        Hat: Just Tricorne
        ...
        Age: 42

~~~

... and see that now it is really easy to undress our pirates with:

~~~haskell

> undress pirate = pirate & attributes . body . hat .~ Nothing
>                         & attributes . body . beard .~ Nothing
>                         & attributes . body . torso .~ Naked
>                         & attributes . body . accessories .~ []

~~~

So we have getters and setters, but we want more we want to use functions, this
is where the `over` function or the `(%~)`-operator comes into play.

~~~haskell

GHCi> hel & attributes . name %~ map toUpper 
The infamous Captain HELLSCREAM of the SS Sea Serpent
        Hat: Just Tricorne
        ...
        Age: 42

GHCi> hel & attributes . age %~ (+10)
The infamous Captain Hellscream of the SS Sea Serpent
        Hat: Just Tricorne
        ...
        Age: 52

~~~


