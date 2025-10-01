size(100).
%move_right(X1,Y1,X2,Y2):- Y2 is Y1, size(S), X1 < S, X2 = X1 + 1.
%move_left(X1,Y1,X2,Y2):- Y2 is Y1, X1 > 1, X2 = X1 - 1.
move_up(X1,Y1,X2,Y2):- X2 is X1, size(S), Y1 < S, Y2 = Y1 + 1.
%move_down(X1,Y1,X2,Y2):- Y2 is Y1, Y1 > 1, Y2 = Y1 - 1.