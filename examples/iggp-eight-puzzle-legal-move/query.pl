legal_move(V0,V1,V2,V3):- succ(V5,V2),input_move(V1,V2,V5),true_cell(V0,V5,V3,V4),cell_type_b(V4).
legal_move(V0,V1,V2,V3):- succ(V2,V5),input_move(V1,V2,V5),true_cell(V0,V5,V3,V4),cell_type_b(V4).
legal_move(V0,V1,V2,V3):- succ(V3,V4),input_move(V1,V2,V4),true_cell(V0,V2,V4,V5),cell_type_b(V5).
legal_move(V0,V1,V2,V3):- cell_type_b(V5),true_cell(V0,V2,V4,V5),input_move(V1,V2,V4),succ(V4,V3).

