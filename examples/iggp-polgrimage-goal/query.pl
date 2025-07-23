goal(V0,V1,V2):- int_0(V2),true_phase(V0,V1,V3),phase_list_build_terrain(V3).
goal(V0,V1,V2):- int_0(V2),true_phase(V0,V1,V3),phase_list_place_pilgrim(V3).
goal(V0,V1,V2):- int_0(V2),height_end(V5),true_pilgrim(V0,V1,V3,V4),true_cell(V0,V3,V4,V5).
goal(V0,V1,V2):- int_10(V2),true_pilgrim(V0,V1,V4,V3),mypos_4(V3),true_cell(V0,V4,V5,V3).
