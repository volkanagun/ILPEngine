f(V0,V1):- empty(V0),empty(V1).
f(V0,V1):- head(V0,V3),tail(V0,V4),f(V4,V2),my_append(V2,V3,V1).
