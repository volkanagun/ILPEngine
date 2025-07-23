f(V0,V1,V2):- one(V1),tail(V0,V2).
f(V0,V1,V2):- decrement(V1,V3),f(V0,V3,V4),tail(V4,V2).
