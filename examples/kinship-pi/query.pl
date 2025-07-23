inv1(V0,V1):- mother(V0,V1).
inv1(V0,V1):- father(V0,V1).
grandparent(V0,V1):- inv1(V0,V2),inv1(V2,V1).
