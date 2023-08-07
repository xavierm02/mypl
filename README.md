The goal of this project is to experiment with frames, i.e. records with some fields left empty, as the basis of an untyped (for now) programming language. See https://xavier.montillet.ac/talks/TYPES-2019/ for an explanation of why frames are useful in a dependently typed setting.


The code is split into three libs:

- mypl contains my experiments with frames.

- modular-calculi allows combining independent calculi, and in particular adding integers to a candidate frame calculus to make testing it easier. It is slightly overengineered, mainly because I wanted to have fun with GADTs and primitive modules.

- mycore contains things that I would have liked to find in Janestreet's Core library.


Questions I would like to eventually provide an answer to are:

- How efficiently can frames be implemented? Is hash-consing necessary? Are co-de-Bruijn indices relevant?

- What can frames simulate? In particular, can a call-by-value variant of frames be used to simulate pattern-matching (somewhat efficiently)?
