---
title: Record Syntax, Lenses and Prisms: Part 3 - Prisms
description: Prisms - a solution for the shanty problem
tags: haskell, eng
---

Prisms
======

So what are prisms?
-------------------

Prisms are like lenses just for sum types ... - the most memorable sentence, but
I don't think very helpful at first glance.

So let us start with defining a sum type.

<!--

> {-# LANGUAGE TemplateHaskell #-}

> module Pirates where
> import Data.Char (toLower, toUpper)
> import Data.List (intercalate)

> import Control.Lens

> data Human = Attributes { _name :: String, _body :: Body, _age :: Int} deriving (Show)
> data Body = Body { _hat :: Maybe Hat
>                  , _beard :: Maybe Beard
>                  , _torso :: Torso
>                  , _accessories :: [Accessories]}

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

Sums
----

Why is the above called a sum type, just let us calculate the number of options
we have in a sum type:

~~~haskell

data Sum = One | Two | Three

~~~

If I have a variable `x :: Sum` it has exactly three values it can be

- `x = One`
- `x = Two`
- `x = Three`

so the sum of the number of constructors is the solution of how many options `x`
can be.

Product
-------

So how come product types to their names - you might already guess it.

~~~haskell

data Product = P Sum Sum

~~~

For a variable `y :: Product` we find `y` can be `|Sum|×|Sum|` many possible
values:

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

The answer is functions

~~~haskell

data Two = One | Two
data THREE = ONE | TWO | THREE

~~~

So the functions from `Two -> THREE` have exactly `|Two|^|THREE|` many elements.

~~~haskell

one,two,three,four,five,six,seven,eight :: Two -> THREE
one   One = ONE
one   Two = ONE
two   One = TWO
two   Two = TWO
three One = THREE
three Two = THREE
four  One = ONE
four  Two = TWO
five  One = ONE
five  Two = THREE
six   One = TWO
six   Two = THREE
seven One = THREE
seven Two = ONE
eight One = THREE
eight Two = TWO

~~~

Back to Prisms
==============

The problem with lenses is that `cpt` some getters don't make sense. For
datatypes that have a special elements, so called monoids like `String` with `""`
we get special getters, but for `Town` for example ...

~~~haskell

Prelude > cpt^.shanty
""
Prelude > cpt^.hometown
...
error message complaining about Town not being a monoid
...

~~~

So an easy way to get *special* elements is combining it with `Maybe`. And
that's what prisms are - getters with `Maybe`-values instead of errors.

So how do we get them - again as with prisms we (have to) build them with
template haskell magic using

~~~haskell

> makePrisms ''Human
> makePrisms ''Body
> makePrisms ''Pirate

~~~

This magic words create "Constructors" `_Captain`, `_FirstMate` and `_Marauder`
and the lens library provides functions `preview`, `review` and `^?`.

<!--

> instance Show Pirate where
>   show (Captain a s)   =  "The infamous Captain "++ a^.name++" of the "++ s ++"\n"
>                        ++ show (a^.body)++"\n"
>                        ++ "\t Age: "++show (a^.age)
>   show (FirstMate a s) =  "Mate "++a^.name++" sings "++s
>                        ++ show (a^.body)++"\n"
>                        ++ "\t Age: "++show (a^.age)
>   show (Marauder a h) =   "Fearsome Pirate "++a^.name++" from "++show h++"\n"
>                        ++ show (a^.body)++"\n"
>                        ++ "\t Age: "++show (a^.age)

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
>                , _hometown = Hipsterhausen
>                }

-->

Now what are those functions doing - they select one *branch* in a sum type.

~~~haskell

Prelude > preview _Captain cpt
Just (Attributes {...},"SS Sea Serpent")
Prelude > preview _Captain mrd
Nothing

~~~

But there is also an infix shortcut for `preview` - `(^?)` so we could have
written the above `cpt^._Captain` or use it with accessor-like functions as
`hometown`,`ship`,`shanty` or `attributes`.

~~~haskell

Prelude > cpt^?hometown
Nothing
Prelude > cpt^?ship
Just "SS Sea Serpent"
Prelude > mrd^?hometown
Just Hipsterhausen

~~~

So what about `review`?
-----------------------

The `review` function can create new things from the results of `preview`, well
not exactly but almost and with use of `(<$>)` from `Control.Applicative` we
can make them work together


~~~haskell

Prelude> :t preview _Captain cpt
preview _Captain cpt :: Maybe (Human, String)
Prelude> let Just x = preview _Captain cpt
Prelude> :t review _Captain x
review _Captain x :: Pirate
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

So Prisms do not fix everything - but provide a safety layer for simple
accessing stuff and sometimes for generating stuff as well.

So thats all I know about Lenses and Prisms - for understanding the type
signatures - I still do not feel confident to present about.
