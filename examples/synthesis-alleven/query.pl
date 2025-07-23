f(V0):- empty(V0).
f(V0):- tail(V0,V1),head(V0,V2),even(V2),f(V1).
