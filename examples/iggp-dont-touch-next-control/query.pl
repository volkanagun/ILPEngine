next_control(V0,V1):- agent_white(V1),true_control(V0,V2),agent_black(V2).
next_control(V0,V1):- agent_black(V1),true_control(V0,V2),agent_white(V2).
