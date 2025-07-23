next_score(V0,V1,V2):- my_true_score(V0,V1,V2),does(V0,V1,V3),end_game(V3).
next_score(V0,V1,V2):- lay_claim(V4),does(V0,V1,V4),my_succ(V2,V3),my_true_score(V0,V1,V3).
next_score(V0,V1,V2):- my_true_score(V0,V1,V2),lay_claim(V4),does(V0,V3,V4),opponent(V1,V3).
