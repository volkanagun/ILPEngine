next_control(V0,V1):- agent_cyan(V1),agent_orange(V2),true_control(V0,V2).
next_control(V0,V1):- agent_blue(V1),agent_red(V2),true_control(V0,V2).
next_control(V0,V1):- agent_red(V1),agent_cyan(V2),true_control(V0,V2).
next_control(V0,V1):- agent_orange(V1),agent_blue(V2),true_control(V0,V2).
