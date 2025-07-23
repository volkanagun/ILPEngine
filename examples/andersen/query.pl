pt(V0,V1):- addr(V0,V1).
pt(V0,V1):- addr(V2,V1),assgn(V0,V3).
pt(V0,V1):- addr(V2,V0),addr(V3,V1).
pt(V0,V1):- load(V0,V3),addr(V2,V1).
