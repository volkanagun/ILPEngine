zendo(V0):- large(V2),piece(V0,V1),size(V1,V2),blue(V1),contact(V1,V3).
zendo(V0):- large(V2),piece(V0,V1),size(V1,V2),contact(V1,V3),rhs(V3).
zendo(V0):- piece(V0,V3),blue(V3),coord1(V3,V1),piece(V0,V2),coord1(V2,V1),red(V2).
