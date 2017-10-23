findall(X-Y, validMove(J, X, Y, NewBoard), L).
setof(V-X-Y, (validMove(J, X, Y, NewBoard), evaluateBoard(NewBoard, J)), L).

playBot:-
    write('----TODO: playBot is not done\n').