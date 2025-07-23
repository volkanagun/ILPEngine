f(V0,V1):- tail(V0,V2),element(V2,V1),head(V0,V1).
f(V0,V1):- tail(V0,V2),f(V2,V1).
