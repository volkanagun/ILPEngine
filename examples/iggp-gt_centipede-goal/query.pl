goal(V0,V1,V2):- true_blackPayoff(V0,V2),int_15(V2),agent_black(V1),true_control(V0,V3),agent_white(V3).
goal(V0,V1,V2):- agent_black(V1),true_control(V0,V1),true_blackPayoff(V0,V2),succ(V2,V3),true_whitePayoff(V0,V3).
goal(V0,V1,V2):- int_0(V2),true_control(V0,V1),true_blackPayoff(V0,V4),succ(V6,V4),succ(V3,V6),succ(V5,V3).
goal(V0,V1,V2):- int_0(V2),role(V1),true_control(V0,V3),agent_white(V3),true_whitePayoff(V0,V4),succ(V5,V4).
goal(V0,V1,V2):- int_0(V2),agent_white(V1),true_blackPayoff(V0,V4),succ(V6,V4),succ(V3,V6),succ(V5,V3).
goal(V0,V1,V2):- agent_white(V1),true_whitePayoff(V0,V2),succ(V3,V2),true_blackPayoff(V0,V3),agent_black(V4),true_control(V0,V4).

