label(V0):- zn(V2),atom(V1,V0,V2).
label(V0):- cu(V2),atom(V1,V0,V2).
label(V0):- c(V4),connected(V3,V5,V2),atom(V5,V0,V4),p(V1),atom(V3,V0,V1).
label(V0):- connected(V3,V5,V2),atom(V5,V0,V4),p(V1),h(V4),atom(V3,V0,V1).
