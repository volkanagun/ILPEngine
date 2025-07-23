f(V0,V1):- empty(V1),tail(V0,V1).
f(V0,V1):- tail(V0,V2),head(V0,V4),f(V2,V3),append(V4,V3,V1).
