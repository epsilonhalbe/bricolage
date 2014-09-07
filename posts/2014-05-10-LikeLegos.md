---
title: Ein bisschen wie LEGO<sup>®</sup>
description: Ein Vergleich: Haskell und Lego - wer hätte gedacht, dass diese beiden so viel gemeinsam haben.
tags: haskell, deu
---
<!--
<link rel="stylesheet" href="highlight.js/styles/solarized_light.css">
 <link rel="stylesheet" href="reveal.js/css/reveal.css"/>
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
-->

Intro
=====

Warum Haskell wie Lego?
-----------------------

Beim Vorbereiten meines Vortrags habe ich überlegt wie ich Leuten so etwas
abstraktes wie Haskell erkläre, ohne dass ich Programmierkenntnisse voraussetze.
Dabei ist mir beim Thema "Immutability" eine Stelle aus "Sophies Welt"
eingefallen, in der das griechische Atommodell mit Lego verglichen wurde.
Ausgehend von dieser Idee sind mir immer mehr Parallelen zwischen diesen beiden
Dingen eingefallen und ihr müsst euch den Blödsinn jetzt anhören.

Schlagwörter
------------

 - Pure
 - Stark typisiert
 - Statisch typisiert
 - Lazy
 - Funktional

Haskell ist "pure"
==================

Pure
----

Wie auch Legosteine sind Haskell Variablen unveränderlich, wer einen Baustein
anderer Form/Farbe benötigt muss den alten wegwerfen (Garbage-Collecten) und
einen neuen Baustein suchen/eine neue Instanz erzeugen.

Das heißt auch, dass Konstrukte und Abstraktionen anderer Programmiersprachen wie
`for`-Schleifen oder `x++` nicht existieren.

Wie löst man ebendiese Probleme?
--------------------------------

 - Rekursion
 - Higher-Order-Functions
 - lokalen Variablen
 - Datenstrukturen wie Listen, Bäumen etc.


Haskell ist stark typisiert
===========================

Stark typisiert
---------------

So wie jeder Legostein eine fixe Länge, Breite und Farbe hat, so besitzt
alles in Haskell einen Typ. `Lego (1 × 4) Red` ist vom Typ Lego (geschrieben
`:: Lego`). Diese Typen werden vom Compiler überprüft, d.h. wo in meinem
Quellcode ein `Lego` erwartet wird kann ich keinen `Duplo` Baustein verwenden.

Beispiele
---------

Ein paar Beispiele die man am besten im Interpreter `ghci` ausprobiert. Also
unter MSWindows `winghci.exe` ausführen oder unter Unix ein Terminal öffnen und
`ghci` aufrufen.

--------------------------------------------------------------------------------

```haskell
Prelude> let a = 'a'
Prelude> :type a
a :: Char
Prelude> let b = ['T','e','x','t']
Prelude> :t b
b :: [Char]
Prelude> let c = "Text"
Prelude> :t c
c :: [Char]
Prelude> b == c
True
Prelude> :t b == c
b == c :: Bool
```

Haskell ist statisch typisiert
==============================

Statisch typisiert
------------------

Wo in anderen Programmiersprachen der Buchstabe `'a'` auch als Zahl verwendet
werden kann und man `'a'+3` berechnen kann und `'d'` erwartet, erhält man in
Haskell eine Fehlermeldung, dass der Operator `(+)` zwei Zahlen vom gleichen Typ
erwartet.

--------------------------------------------------------------------------------

Auch die `putStrLn`-Funktion erwartet einen `String`, den sie auf der
Kommandozeile ausgibt und macht **keine** implizite Umwandlung, d.h.

```haskell
Prelude> putStrLn 3
--lange Errormessage
```

Alles hat einen Typ
-------------------

Auch Funktionen haben einen und der wird in einer Datei z.B. `MyFile.hs` meistens
dazugeschrieben:

```haskell
f :: Int -> Int -> Int
f x y = 2 * x + y
```

 - `f :: Int -> Int -> Int`<br>
   heißt: `f` nimmt einen `Int` und auch einen zweiten `Int` und liefert
   (das ist immer das letzte Dings in so einer Zeile) einen `Int`
 - man verwendet Klammern nur da wo sie notwendig sind

Eigene Typen
------------

Haskell erlaubt es auch sich eigene Typen auszudenken.

```haskell
data Color = Black | Red     | Green | Yellow
           | Blue  | Magenta | Cyan  | White
data Dimension = Dim {x :: Int, y :: Int}
data Lego = Lego {dim :: Dimension, color :: Color}
```


Haskell ist funktional
======================

Funktional
----------

Funktionen haben keinen besonderen Status, man kann sie wie jeden anderen Typ in
Variablen speichern, in Listen packen oder als Parameter in anderen Funktionen
verwenden.


WTF ist Funktional in LEGO
--------------------------

Baupläne! Baupläne sind genauso wie Haskell-Funktionen, immer gleich. Soll
heißen wenn ich einen Bauplan zwei mal hintereinander ausführe kommt das geiche
Modell heraus bzw. bei Haskell der gleiche Wert.

Weiters werden in Baupläne kleine Teilbaupläne verwendet - auch in
Haskell-Funktionen werden kleinere Funktionen als "Bausteine" verwendet.

Beispiele
---------

```haskell
Prelude> let a x = 10*x
Prelude> a 10
100
Prelude> let b = (+)
Prelude> b 1 2
3
Prelude> let c = \x -> x*2
Prelude> c 1
2
Prelude> let d = \x y -> x*y
Prelude> d 2 3
6
```

Haskell ist "lazy"
==================

Lazy
----

So wie beim Bau eines Lego-Hauses, wo ich ein Bauteil erst dann suche wenn ich es
benötige, wird auch in Haskell Zeug erst dann ausgewertet wenn es gebraucht wird.

--------------------------------------------------------------------------------

```haskell
Prelude> let a = 3
Prelude> :sprint a
a = 3
Prelude> let b = [1..10]
Prelude> :sprint b
b = _
Prelude> let c = map (*2) b
Prelude> :sprint c
c = _
Prelude> length c
Prelude> :sprint b
b = [1,2,3,4,5,6,7,8,9,10]
Prelude> :sprint c
c = [_,_,_,_,_,_,_,_,_,_]
```

Nochmal lazyness in Action
--------------------------

```haskell
Prelude> let fib = 1:1:zipWith (+) fib (tail fib)
Prelude> take 10 fib
[1,1,2,3,5,8,13,21,34,55]
```

Noch einmal Legos
=================

GHCi
----

```bash
$> ghci LikeLegos.hs
```


```haskell
let lego = Lego (1×4)
let colors = [Black .. White]
map lego colors
pprint $ map lego colors
pprint $ map lego (colors ++ colors)
pprint $ map lego (colors ++ reverse colors)
pprint $ map lego (colors ++ tail (reverse colors))
let legos = map lego (colors ++ tail (reverse colors))
```

--------------------------------------------------------------------------------

```haskell
pprint $ map (setX 2) $ legos'
pprint $ map (setColor Red . setX 2) legos'
pprint $ zipWith setY [1..] (map (setColor Red) legos')
pprint $ zipWith setY ([1..8]++[7,6..1]) legos'
pprint $ zipWith setY (map (\x -> 9-x) ([1..8]++[7,6..1])) legos'
```

Was brauche ich zum herumspielen?
=================================

Compiler
--------

 - Einen funktionierenden Compiler/Interpreter am besten Haskell-Platform
[herunterladen](http://www.haskell.org) und installieren
 - Compiler gibt es mehrere, der mit Abstand populärste ist GHC
 - Interpreter gibt es auch einige, aber außer GHCi sind alle veraltet

Editor
------

 - Vim + Plugins (Syntastic, ghc-mod, haskellmode, hdevtools, lushtags)
 - Emacs + Plugins (ghc-mod)
 - Eclipse + EclipseFP
 - FPComplete hat einen online editor

Bücher
------

 - [Learn you a haskell for great good](http://learnyouahaskell.com) (short lyah)
 - [Real World Haskell](http://book.realworldhaskell.org/read/)
 - [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)
 - [Haskell and Yesod](http://www.yesodweb.com/)

Blogs & Podcasts
----------------

 - [Planet Haskell](http://planet.haskell.org/) eine Sammlung von aktuellen Blogartikeln
 - [Reddit](http://www.reddit.com/r/haskell)
 - [Haskellcast](http://www.haskellcast.com/)
 - haskell-cafe - mailinglist
 - [Lambdaheads](https://metalab.at/wiki/Lambdaheads)

Sonstige Tools
--------------

 - HLint - ein ausgezeichnetes Tool das Codestyle verbessert
 - [Hackage](http://hackage.haskell.org/) - das zentrale Code-Repository
 - Cabal - der Paketmanager von Haskell
 - Hoogle - eine Suchmaschine die es ermöglicht nach Funktionen zu suchen
 - Hayoo - noch eine Suchmaschine, die noch mehr Pakete durchsucht
 - [www.stackoverflow.com](http://stackoverflow.com/questions/tagged/haskell)
