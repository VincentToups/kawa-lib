Kawa Lib
========

A set of utilities for programming in Kawa Scheme. So far somewhat
unfocused but may contain useful things for other developers.

Highlights
----------

Probably the most useful thing the port of the pattern matcher
Shadchen.

```
kawa -Dkawa.import.path="`pwd`/*.scm"
#|kawa:1|# (import (lib shadchen))
#|kawa:2|# (match (list 1 2 3) ((list a b c) (list c b a)))
(3 2 1)
```

So far this version of shadchen does not provide extensible patterns
because I'm not sure how to integrate it with R7RS style
define-library forms in a sensible way.

Mecs
----

I'm primarily interested in game development with Scheme. To that end,
this library contains an entity-component-system framework called
"mecs".

Utils
-----

There are some misc syntactic utilities. 

```
(import (lib util))
(.. some-object x (some-method a b c))
; translates to
; ((some-method:x):some-method a b c)

```

