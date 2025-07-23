next_score(V0,V1,V2):- my_true_score(V0,V1,V2),does(V0,V1,V5),beats(V3,V5),does(V0,V4,V3).
next_score(V0,V1,V2):- my_true_score(V0,V1,V2),does(V0,V1,V3),different(V1,V4),does(V0,V4,V3).
next_score(V0,V1,V2):- my_true_score(V0,V1,V5),my_succ(V5,V2),does(V0,V1,V6),beats(V6,V3),does(V0,V4,V3).

