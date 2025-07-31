next_list(V0,V1):- tail(V0,V2),head(V1,V2),head(V3,V0),x(V3).
next_list(V0,V1):- tail(V0,V2),next_list(V2,V1).
