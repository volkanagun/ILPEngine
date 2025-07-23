f(V0):- tail(V0,V1),tail(V1,V2),empty(V2).
f(V0):- head(V0,V2),tail(V0,V3),head(V3,V1),geq(V1,V2),f(V3).
