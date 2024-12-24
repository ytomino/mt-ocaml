Mersenne Twister collection library for Objective-Caml
======================================================

What's this?
------------

This library has the several Mersenne Twister pseudo-random number generators
imported to OCaml.

mt19937ar
 | https://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/MT2002/mt19937ar.html
  (日本語)
 | https://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/MT2002/emt19937ar.html
  (English)
SFMT
 https://github.com/MersenneTwister-Lab/SFMT/
dSFMT
 https://github.com/MersenneTwister-Lab/dSFMT/

Prerequisites
-------------

OCaml >= 4.11
 https://ocaml.org/

How to make
-----------

Install
+++++++

::

 make install PREFIX=/usr/local

Specify your preferred directory to ``PREFIX``.
The libraries would be installed into ``$PREFIX/lib/ocaml`` (default is
``ocamlc -where``).

Uninstall
+++++++++

::

 make uninstall PREFIX=/usr/local

Build examples
++++++++++++++

::

 make -C examples

Note
----

From bit sequence
+++++++++++++++++

This library uses division instead of remainder to get an uniformly distributed
value from a sequence of bits.
Especially in the case of a power of 2, it is equivalent to extracting higher
bits.
Therefore, the consistent results can be gotten across the multiple functions.

Range of float
++++++++++++++

The range of results of ``Stdlib.Random.float bound`` has changed along with
the version of OCaml.
It is [0,\ ``bound``) before OCaml 3.12, [0,\ ``bound``] between OCaml 4.00 to
4.14, and (0,\ ``bound``) after OCaml 5.0.

All ``float`` functions in this library return values in the range
[0,\ ``bound``).
