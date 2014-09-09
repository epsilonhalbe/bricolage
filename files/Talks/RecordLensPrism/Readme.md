% Record Syntax, Lenses and Prisms
%Martin Heuschober;
 [CC-BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/)
%10. Mai 2014

<link rel="stylesheet" href="highlight.js/styles/solarized_light.css">
 <link rel="stylesheet" href="reveal.js/css/reveal.css"/>
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>

Pirates and Records
===================

Algebraic Data Types
--------------------

The haskell equivalent of objects are **A**lgebraic **D**ata **T**ypes or ADTs
for short.

~~~haskell
data Pirate = Captain | FirstMate | Marauder deriving (Show)
~~~

--------------------------------------------------------------------------------

- `data`
    - lhs type
    - rhs "constructors"
- `deriving`
- `Show`

Records
-------

~~~haskell
data Pirate = Captain   { name :: String, ship :: String}
            | FirstMate { name :: String, shanty :: String}
            | Marauder  { name :: String, hometown :: String}

instance Show Pirate where
  show (Captain n s) = "Captain "++n++" of the "++s
  show (FirstMate n s) = "Mate "++n++" sings "++s
  show (Marauder n h) = "Fearsome Pirate "++n++" from "++h
~~~

--------------------------------------------------------------------------------

- ... `{name :: String, ...}` - record one way to have a functional getter/setter
- `name` is a function from `Pirate` to `String`
- in addition Haskell provides special syntax to *update* with
  `pirate {name = "New Name"}`

Some Examples
-------------

~~~haskell
cpt = Captain "Blackbeard" "SS Sea Serpent"
mt1 = FirstMate {name = "Redbeard",
                 shanty = "What shall we do with the drunken sailor"}
mrd = Marauder {name = "Neckbeard"}
crw = map (\t -> mrd {hometown = t})
            ["Yorkshire", "Jamestown", "Moscow", "Port-au-Prince"]
pirates = [cpt,mt1]++crw

cpt' = cpt {name = "Greybeard"}
=> "Captain Greybeard of the SS Sea Serpent"
~~~

More complicated records
------------------------

~~~haskell
data Human = Attributes { name :: String, body :: Body, age :: Int}
           deriving (Show)
data Body = Body { hat :: Maybe Hat
                 , beard :: Maybe Beard
                 , torso :: Torso
                 , accessories :: [Accessories]}
~~~

--------------------------------------------------------------------------------

~~~haskell
data Hat = Tricorne | WideBrimmedHat | Bandana deriving (Show)
data Torso = Naked | Vest | ShabbyShirt deriving (Show)
data Accessories = Parrot | Monkey | PegLeg | EyePatch | EarRing | Hook deriving (Show)
data Beard = Beard Colour BeardType deriving (Show)
data Colour = Black | Red | Blond | White | Brown deriving (Show)
data BeardType = Moustache | Ladybeard | Goatee | FullBeard deriving (Show)
~~~

--------------------------------------------------------------------------------

~~~haskell
data Pirate = Captain   { attributes :: Human, ship :: String}
            | FirstMate { attributes :: Human, shanty :: String}
            | Marauder  { attributes :: Human, hometown :: String}

instance Show Pirate where
  show (Captain a s)   =  "The infamous Captain "++ name a++" of the "++s++"\n"
                       ++ show (body a)++"\n"
                       ++ "\t Age: "++show (age a)
~~~
~~~

Now some more Examples
----------------------

~~~haskell
cpt = Captain ( Attributes  "Blackbeard"
                           ( Body ( Just Tricorne)
                                  ( Just (Beard Black FullBeard))
                                    Vest
                                    [Parrot, PegLeg]
                                  )
                            42)
                "SS Sea Serpent"
~~~

Without the names records are barely readable.

--------------------------------------------------------------------------------

~~~haskell
mt1 = FirstMate { attributes =
                    Attributes { name = "Redbeard"
                               , body = Body { hat   = Just WideBrimmedHat
                                             , beard = Nothing
                                             , torso = Naked
                                             , accessories = [EarRing, Monkey]
                                             }
                               , age = 30
                               }
                , shanty = "What shall we do with the drunken sailor"
                }
~~~

With names they are verbose but readable.

--------------------------------------------------------------------------------

~~~haskell
mrd = Marauder { attributes =
                    Attributes { name = "Neckbeard "
                               , body = Body { hat   = Just Bandana
                                             , beard = Just (Beard Brown Goatee)
                                             , torso = ShabbyShirt
                                             , accessories = [EyePatch]
                                             }
                               , age = 20
                               }
                 -- note hometown is missing
                }
~~~

And now the tricky part
-----------------------

~~~haskell
crw = map (\(n,t) -> mrd { attributes = (attributes mrd) {name = (name.attributes) mrd ++  n}
                         , hometown   = t})
          [("Joe"  , "Yorkshire"     )
          ,("Jack" , "Jamestown"     )
          ,("Igor" , "Moscow"        )
          ,("Maria", "Port-au-Prince")]
~~~

And that's one reason why you don't like to do stuff with record syntax and
deeply nested data structures

But then there comes Edward Kmett's lens library to the rescue.

Lenses
======

History
-------

Lenses are a quite interesting idea first mentioned by Twan van Laarhoven and
have lived through a few implementations until the `lens`-library by Ed Kmett
has proven to be the stable solution for now. It has a *batteries included*
approach and provides many operators and a template haskell convention to
generate lenses for your own algebraic data types.

How stuff works
---------------

The convention is to put an `_` at the beginning of the record names in the
record syntax definition. And then use the magic of template haskell to generate
the corresponding lenses a.k.a. functional getters and setters
with `makeLenses ''MyADT`.

Ingredients
-----------

For one we need the template haskell language pragma to make the magic work.

~~~haskell
{-# LANGUAGE TemplateHaskell #-}
~~~

Installation
------------

And we also need the lens library to be installed for which I recommend using
`cabal`, the interface to the haskell packaging system.

~~~shell
~ $ cabal update
~ $ cabal install lens
... this may take some time
Installed lens-4.3.3
~~~

Import and Definition
---------------------

Next step is to do the import of the lens package and add a whole bunch of
underscores ...

~~~haskell
import Control.Lens

data Human = Attributes { _name :: String, _body :: Body, _age :: Int} deriving (Show)
data Body = Body { _hat :: Maybe Hat
                 , _beard :: Maybe Beard
                 , _torso :: Torso
                 , _accessories :: [Accessories]}
~~~

--------------------------------------------------------------------------------

~~~haskell
data Pirate = Captain   { _attributes :: Human, _ship :: String}
            | FirstMate { _attributes :: Human, _shanty :: String}
            | Marauder  { _attributes :: Human, _hometown :: String}
~~~

... and of course create the lenses. (In the background template haskell now
creates functions attributes, ship, shanty and so on.)

Abracadabra
-----------

~~~haskell
makeLenses ''Human
makeLenses ''Body
makeLenses ''Pirate
~~~

And "Show"
----------

~~~haskell
instance Show Pirate where
  show (Captain a s)   =  "The infamous Captain "++ a^.name++" of the "++ s++"\n"
                       ++ show (a^.body)++"\n"
                       ++ "\t Age: "++show (a^.age)
~~~

So what is this `^.` operator, it is an alias for the `view`-function
that can focus on the parts of a lens.

Finally type signatures
-----------------------

The type signature for `(^.)` is a bit complicated, but if we combine it with
the generated functions we see

~~~haskell
GHCi> :t (^.attributes)
(^.attributes) :: Pirate -> Human
GHCi> :t (^.attributes.name)
(^.attributes.name) :: Pirate -> String
~~~

More type signatures
--------------------

But where the combined type signature of `(^.)` and `attributes` is simple their
own type signature is - let's just call it *not* simple.

~~~haskell
GHCi> :t (^.)
(^.) :: s -> Getting a s a -> a
GHCi> :t attributes
attributes :: Functor f => (Human -> f Human) -> Pirate -> f Pirate
~~~

--------------------------------------------------------------------------------

As we saw we have something called `Getting` so there should also be some
setting stuff and indeed we have a `set`-function and the infix alias `(.~)`
which is more useful with the `&`-operator which is just reverse function
application: `x & f = f x`.

A few examples should provide a bit more insight...

Lens examples
-------------

~~~haskell
hel = cpt & attributes.name.~ "Hellscream"

GHCi > hel
The infamous Captain Hellscream of the SS Sea Serpent
        Hat: Just Tricorne
        ...
        Age: 42
~~~

Concatenating Lenses
--------------------

... and see that now it is really easy to undress our pirates with:

~~~haskell
undress pirate = pirate & attributes . body . hat .~ Nothing
                        & attributes . body . beard .~ Nothing
                        & attributes . body . torso .~ Naked
                        & attributes . body . accessories .~ []
~~~

Over and
--------

So we have getters and setters, but we want more we want to use functions, this
is where the `over` function or the `(%~)`-operator comes into play.

--------------------------------------------------------------------------------

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

Prisms
======

So what are prisms?
-------------------

Prisms are like lenses just for sum types ... - the most memorable sentence, but
I don't think very helpful at first glance.

So let us start with defining a sum type.

--------------------------------------------------------------------------------

~~~haskell

> data Town = Prague | Vienna | Kingston | London | Hipsterhausen deriving (Show, Eq)

> data Pirate = Captain   { _attributes :: Human, _ship :: String}
>             | FirstMate { _attributes :: Human, _shanty :: String}
>             | Marauder  { _attributes :: Human, _hometown :: Town}

> makeLenses ''Human
> makeLenses ''Body
> makeLenses ''Pirate

~~~

Pirate is a sum type - with different records in each constructor.

A tangent on sums and products
==============================

Why is the above called a sum type?
-----------------------------------

Begin with defining

~~~haskell

data Sum = One | Two | Three

~~~

then `x :: Sum` has exactly three values it can assume.

--------------------------------------------------------------------------------

Namely

- `x = One`
- `x = Two`
- `x = Three`

So how come product types to their names?
-------

You might already guess it.

~~~haskell

data Product = P Sum Sum -- equivalent to (Sum, Sum)

~~~

A variable `y :: Product` can have `|Sum|×|Sum|` many possible values:

--------------------------------------------------------------------------------

- `y = P One One`
- `y = P One Two`
- `y = P One Three`
- `y = P Two One`
- `y = P Two Two`
- `y = P Two Three`
- `y = P Three One`
- `y = P Three Two`
- `y = P Three Three`

So can anybody find what exponential types are?
-----------------------------------------------

The answer is
-------------

### Functions

--------------------------------------------------------------------------------

Let us define

~~~haskell

data Two = One | Two
data THREE = ONE | TWO | THREE

~~~

Then the functions from `Two -> THREE` have exactly `|Two|^|THREE|`
many elements.

--------------------------------------------------------------------------------

~~~haskell

one,two,three,four,five,six,seven,eight :: Two -> THREE
one   One = ONE    | five  One = ONE
one   Two = ONE    | five  Two = THREE
two   One = TWO    | six   One = TWO
two   Two = TWO    | six   Two = THREE
three One = THREE  | seven One = THREE
three Two = THREE  | seven Two = ONE
four  One = ONE    | eight One = THREE
four  Two = TWO    | eight Two = TWO

~~~

Back to
=======

Prisms
------

The problem with lenses is that for `cpt :: Pirate` some getters don't make
sense, like `cpt^.shanty` or `mrd^.ship`.

There is a group of structures that have a neutral/zero/one element - `String`
has `""`, `List` has `[]` - one common abstraction over this would be `Monoid`.

But for others like `Town` we have no such candidate.

--------------------------------------------------------------------------------

~~~haskell

Prelude > cpt^.shanty
""
Prelude > cpt^.hometown
...
error message complaining about Town not being a monoid
...

~~~

--------------------------------------------------------------------------------

So an easy way to get *special* elements is combining it with `Maybe`. And
that's what prisms are - getters with `Maybe`-values instead of errors.

So how do we get them - again as with prisms we (have to) build them with
template haskell magic using

~~~haskell

> makePrisms ''Human
> makePrisms ''Body
> makePrisms ''Pirate

~~~

--------------------------------------------------------------------------------

This magic words create "constructors" `_Captain`, `_FirstMate` and `_Marauder`
and the lens library provides functions `preview`, `review` and `^?`.

Now what are those functions doing - they select one *branch* in a sum type.

~~~haskell

Prelude> preview _Captain cpt
Just (Attributes {...},"SS Sea Serpent")
Prelude> preview _Captain mrd
Nothing

~~~

--------------------------------------------------------------------------------

But there is also an infix shortcut for `preview` - `(^?)` so we could have
written the above `cpt^._Captain` or use it with accessor-like functions as
`hometown`,`ship`,`shanty` or `attributes`.

~~~haskell

Prelude> cpt^?hometown
Nothing
Prelude> cpt^?ship
Just "SS Sea Serpent"
Prelude> mrd^?hometown
Just Hipsterhausen

~~~

So what about `review`?
-----------------------

The `review` function can create new things from the results of `preview`, well
not exactly but almost and with use of `(<$>)` from `Control.Applicative` we
can make them work together

--------------------------------------------------------------------------------

~~~haskell

Prelude> :t preview _Captain cpt
preview _Captain cpt :: Maybe (Human, String)
Prelude> let Just x = preview _Captain cpt
Prelude> :t review _Captain x
review _Captain x :: Pirate

~~~

--------------------------------------------------------------------------------

~~~haskell

Prelude> :t review _Captain <$> preview _Captain cpt
review _Captain <$> preview _Captain cpt :: Maybe Pirate
Prelude> review _Captain <$> preview _Captain cpt
Just Captain {Attributes {...},"SS Sea Serpent"}
Prelude> review _Captain <$> preview _Captain mrd
Nothing
Prelude> review _Captain <$> preview _Marauder mrd
... Error ... -- Captain needs String (ship name) where Marauder has a Town as
a second argument

~~~

--------------------------------------------------------------------------------

So Prisms do not fix everything - but provide a safety layer for simple
accessing stuff and sometimes for generating stuff as well.

So that's all I know about Lenses and Prisms - for understanding the type
signatures - I still do not feel confident to present about.

Out
---

Thank you for your attention!

