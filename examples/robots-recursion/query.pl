f(V0,V1):- move_up(V0,V2),move_up(V2,V1).
f(V0,V1):- move_up(V0,V2),f(V2,V1).
