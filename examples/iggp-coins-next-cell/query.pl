next_cell(V0,V1,V2):- c_zerocoins(V2),does_jump(V0,V3,V1,V4).
next_cell(V0,V1,V2):- c_twocoins(V2),does_jump(V0,V3,V4,V1).
next_cell(V0,V1,V2):- my_true_cell(V0,V1,V2),does_jump(V0,V5,V3,V4),different(V1,V3),different(V1,V4).

