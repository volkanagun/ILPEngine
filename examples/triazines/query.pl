great(V0,V1):- struc4(V0,V5,V3),struc3(V0,V5,V3),struc3(V1,V5,V4),struc4(V1,V5,V2).
great(V0,V1):- struc3(V0,V4,V2),branch(V4,V5),struc3(V1,V4,V2),struc4(V0,V4,V2),struc4(V1,V3,V2),branch(V3,V5).
great(V0,V1):- struc4(V1,V4,V2),polar(V4,V5),struc3(V1,V4,V2),struc3(V0,V3,V2),struc4(V0,V4,V2),polar(V3,V5).
great(V0,V1):- struc4(V0,V4,V2),h_acceptor(V4,V5),struc3(V1,V4,V2),struc3(V0,V3,V2),h_acceptor(V3,V5),struc4(V1,V4,V2).
great(V0,V1):- struc3(V1,V3,V2),struc4(V1,V3,V2),struc4(V0,V4,V2),struc3(V0,V3,V2),polar(V4,V5),polar(V3,V5).
