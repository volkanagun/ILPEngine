next_list(V0,V1):- tail(V0,V2),head(V2,V1),head(V0,V3),x(V3).
next_list(V0,V1):- tail(V0,V2),next_list(V2,V1).
