move_right(X1,Y1, X2,Y2) :- size(S), equal(R, Y1, Y2),  X1 < S, X2 = X1 + 1.
move_left(X1,Y1, X2, Y2) :- X1 > 1, equal(R, Y1, Y2), X2 = X1 - 1.
move_up(X1, Y1, X2,Y2) :- size(S), equal(R, X1, X2),  Y1 < S, Y2 = Y1 + 1.
move_down(X1,Y1, X2, Y2) :- Y1 > 1, equal(R, X1, X2),  Y2 = Y1 - 1.
at_top(X,Y) :- size(Y).
at_right(X,Y) :- size(X).

f(X0,Y0, X1, Y1):- move_up(X0,Y0, X1, Y1).
f(X0, Y0, X1, Y1):- move_up(X0,Y1, X2, Y2), f(X2,Y2,X1,Y1).
