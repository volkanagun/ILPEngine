right(X0,Y0,X1,Y1):- Y1 is Y0, X1 = X0 + 1.
f(X0,Y0,X1,Y1):- right(X0,Y0, X2,Y2), right(X2,Y2, X1,Y1).
