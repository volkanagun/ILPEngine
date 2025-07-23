ancestor(V0,V1):- father(V2,V1),father(V0,V3).
ancestor(V0,V1):- mother(V0,V3),father(V1,V2).
