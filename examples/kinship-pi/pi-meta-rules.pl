parent(A,B) :- alpha(A,B).
grandparent(A,B) :- parent(A,Z), parent(Z,B).
