# Multinumbers

Multisets (sets with repeating elements) have been used in mathematics,
physics, computer science, and logic.

G.P. Monro distinguishes **multinumbers** and **multisets**

> It is the contention of this paper that the intuitive concept of
> multiset in fact contains two underlying ideas, and that these
> ideas should be separated. One of the resulting concepts is more set-like
> than the other, and the name “multiset” has been appropriated for this
> concept; the other concept is more numeric in character and has been
> named “multinumber ”. It is hoped that distinguishing the concepts
> “multiset ” and “multinumber” will clear up some unsatisfactory
> features in previous treatments of multisets.

There are two important ways to define multisets.

- A carrier set `X` and an equivalence relation on X define a **multiset**.
 Every element in `X` is distinct, the *multiplicity* (occurance count) of
 an element is the size of it's equivalence class.

- A **multinumber** is a collection `Y` which can contain the same element
 multiple times, so multiplicity is given by `Y -> nat`.

This implements the latter, it's based on `purescript-sets`.
