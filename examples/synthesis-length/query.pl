f(V0,V1):- empty(V0),zero(V1).
f(V0,V1):- tail(V0,V3),f(V3,V2),succ(V2,V1).
