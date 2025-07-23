legal(V0,V1,V2):- true_control(V0,V1),action_continue(V2).
legal(V0,V1,V2):- true_control(V0,V1),action_finish(V2).
legal(V0,V1,V2):- agent_black(V1),action_noop(V2),true_control(V0,V3),agent_white(V3).
legal(V0,V1,V2):- action_noop(V2),agent_white(V1),true_control(V0,V3),agent_black(V3).
