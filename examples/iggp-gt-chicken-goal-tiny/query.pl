goal(V0,V1,V2):- true_whiteScore(V0,V2),agent_white(V1).
goal(V0,V1,V2):- true_blackScore(V0,V2),agent_black(V1).
