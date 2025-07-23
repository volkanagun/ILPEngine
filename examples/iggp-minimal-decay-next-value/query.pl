next_value(V0,V1):- c5(V1),does(V0,V3,V2),press_button(V2).
next_value(V0,V1):- my_succ(V1,V2),true_value(V0,V2),does(V0,V4,V3),noop(V3).
