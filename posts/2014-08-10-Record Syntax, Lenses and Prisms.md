---
description: Functional getters and setters and a bit more
tags: haskell, eng
---

Intro
=====

Pirates and Records
-------------------

As a programmer you are often tasked with the problem of modelling reality and
thus your customers have assigned you to make a complex data structure, something
like a pirate captain and a gruesome crew of marauders.

In an object oriented approach one would start with designing a "plain old object"
and inheriting a whole bunch of attributes. The haskell equivalent of objects
are **A**lgebraic **D**ata **T**ypes or ADTs for short.

<!--

> module Pirates where
> import Data.Char (toLower)
> import Data.List (intercalate)

-->

~~~haskell
data Pirate = Captain | FirstMate | Marauder deriving (Show)
~~~

This definition above is quite similar to objects, but it actually defines a
type which consists of three possible *"constructors"*, the last statement
`deriving (Show)` is haskell's way of saying we have a `toString`-method called
`show`.

But your customers want to customize those Pirates (hence the name).
So you decide to come up with a more accurate model of pirates.

~~~haskell

data Pirate = Captain   { name :: String, ship :: String}
            | FirstMate { name :: String, shanty :: String}
            | Marauder  { name :: String, hometown :: String}

instance Show Pirate where
  show (Captain n s) = "Captain "++n++" of the "++s
  show (FirstMate n s) = "Mate "++n++" sings "++s
  show (Marauder n h) = "Fearsome Pirate "++n++" from "++h

~~~

The customers like the prototype - but as they are very unfamiliar with
functional programming they ask you to prepare a little demo.

~~~haskell

cpt = Captain "Blackbeard" "SS Sea Serpent"
mt1 = FirstMate {name = "Redbeard", shanty = "What shall we do with the drunken sailor"}
mrd = Marauder {name = "Neckbeard"}
crw = map (\t -> mrd {hometown = t}) ["Yorkshire", "Jamestown", "Moscow", "Port-au-Prince"]
pirates = [cpt,mt1]++crw

cpt' = cpt {name = "Greybeard"} => "Captain Greybeard of the SS Sea Serpent"

~~~

Still the customers is impressed with the prototype but still not content, so
you start with designing a very detailed model starting with humans.

~~~haskell

> data Human = Attributes { name :: String, body :: Body, age :: Int} deriving (Show)
> data Body = Body { hat :: Maybe Hat
>                  , beard :: Maybe Beard
>                  , torso :: Torso
>                  , accessories :: [Accessories]}

~~~

<!--

> instance Show Body where
>   show b = "\t Hat: "  ++show (hat b)++"\n"
>          ++"\t Beard: "++show (beard b)++"\n"
>          ++"\t Torso: "++show (torso b)++"\n"
>          ++"\t Accessories: "++ (intercalate ", " $ map show (accessories b))

-->

Then you come up with the nitty gritty details like `Hat`, `Torso` and so on.

~~~haskell

> data Hat = Tricorne | WideBrimmedHat | Bandana deriving (Show)
> data Torso = Naked | Vest | ShabbyShirt deriving (Show)
> data Accessories = Parrot | Monkey | PegLeg | EyePatch | EarRing | Hook deriving (Show)

> data Beard = Beard Colour BeardType
> instance Show Beard where show (Beard c t) = "an exquisite "++ map toLower (show c)++" "++show t
> data Colour = Black | Red | Blond | White | Brown deriving (Show)
> data BeardType = Moustache | Ladybeard | Goatee | FullBeard deriving (Show)

> data Pirate = Captain   { attributes :: Human, ship :: String}
>             | FirstMate { attributes :: Human, shanty :: String}
>             | Marauder  { attributes :: Human, hometown :: String}

~~~
<!--

> instance Show Pirate where
>   show (Captain a s)   =  "The infamous Captain "++ name a++" of the "++ s++"\n"
>                        ++ show (body a)++"\n"
>                        ++ "\t Age: "++show (age a)
>   show (FirstMate a s) =  "Mate "++name a++" sings "++s
>                        ++ show (body a)++"\n"
>                        ++ "\t Age: "++show (age a)
>   show (Marauder a h) =   "Fearsome Pirate "++name a++" from "++h++"\n"
>                        ++ show (body a)++"\n"
>                        ++ "\t Age: "++show (age a)

-->

The customer asks for a demo so you make a new crew based on the old examples.

~~~haskell

> cpt = Captain ( Attributes  "Blackbeard"
>                            ( Body ( Just Tricorne)
>                                   ( Just (Beard Black FullBeard))
>                                     Vest
>                                     [Parrot, PegLeg]
>                                   )
>                             42)
>                 "SS Sea Serpent"

~~~

Not really good and readable code so you try it a bit more verbose.

~~~haskell

> mt1 = FirstMate { attributes = Attributes { name = "Redbeard"
>                                           , body = Body { hat   = Just WideBrimmedHat
>                                                         , beard = Nothing
>                                                         , torso = Naked
>                                                         , accessories = [EarRing, Monkey]
>                                                         }
>                                           , age = 30
>                                           }
>                 , shanty = "What shall we do with the drunken sailor"
>                 }

~~~

The last piece - a.k.a. the crew was not too easy in the first example so you
don't expect this to be a piece of cake, well it isn't.

~~~haskell

> mrd = Marauder { attributes = Attributes { name = "Neckbeard "
>                                           , body = Body { hat   = Just Bandana
>                                                         , beard = Just (Beard Brown Goatee)
>                                                         , torso = ShabbyShirt
>                                                         , accessories = [EyePatch]
>                                                         }
>                                           , age = 20
>                                           }
>                 }

~~~

After the initial constructor the tricky part just begins - it takes four tries
and a lot of hard thinking to get the following lambda expression right.

~~~haskell

> crw = map (\(n,t) -> mrd { attributes = (attributes mrd) {name = (name.attributes) mrd ++  n}
>                          , hometown   = t})
>           [("Joe"  , "Yorkshire"     )
>           ,("Jack" , "Jamestown"     )
>           ,("Igor" , "Moscow"        )
>           ,("Maria", "Port-au-Prince")]

~~~

There are signs of bad code in this, a lot of signs - `mrd` is written three
times, it is complicated not only to a programmer new to the haskell world.

But then there comes Edward Kmett's lens library to the rescue.

