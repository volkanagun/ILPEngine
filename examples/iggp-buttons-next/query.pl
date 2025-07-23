next(V0,V1):- my_succ(V2,V1),my_true(V0,V2).
next(V0,V1):- my_true(V0,V1),c_r(V1),does(V0,V3,V2),c_b(V2).
next(V0,V1):- my_true(V0,V1),c_r(V1),c_a(V2),does(V0,V3,V2).
next(V0,V1):- my_true(V0,V1),c_q(V1),c_a(V2),does(V0,V3,V2).
next(V0,V1):- my_true(V0,V1),c_p(V1),c_c(V2),does(V0,V3,V2).
next(V0,V1):- c_p(V1),not_my_true(V0,V1),c_a(V3),does(V0,V2,V3).
next(V0,V1):- c_p(V1),c_b(V3),does(V0,V2,V3),c_q(V4),my_true(V0,V4).
next(V0,V1):- c_r(V1),does(V0,V4,V3),c_c(V3),c_q(V2),my_true(V0,V2).
next(V0,V1):- c_q(V1),c_p(V2),my_true(V0,V2),c_b(V4),does(V0,V3,V4).
next(V0,V1):- c_q(V1),c_c(V4),does(V0,V3,V4),c_r(V2),my_true(V0,V2).

