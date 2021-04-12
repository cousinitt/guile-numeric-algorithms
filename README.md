# guile-numeric-algorithms
A collection of numerical analysis programs, written in GNU Guile.

## Scalar Minimization
This folder contains a collection of scalar minimization routines that are
commonly found in textbooks. If you just wanted to find a local minimum
efficiently, I think that the best one is Brent-Jarratt method (*brent.scm*).

## Multidimensional Minimization
This folder contains (part of an) implementation of a genetic optimiser. Check
back later and I may have completed it. Right now I am not sure that I like the
structure of the code, I had translated it from some Python code that I wrote
years ago.

## Root Finding
This folder contains a collection of scalar root finding routines that are
commonly taught in courses on numerical methods. The best one is from the Boost
library, called Toms748. I have not implemented it yet. Of the algorithms that
remain, the Brent-Dekker (*brent.scm*) algorithm is the best. Most of the
algorithms break up a real interval in order to find the root, and are not
useful for finding complex roots. The Muller method can apparently be used for
finding complex roots (*muller.scm*).
