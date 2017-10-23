
% check if cell is valid (x, y) and print error if not
checkValidCell(X, Y):-isValid(X, Y), !.
checkValidCell(X, Y):-format('Cell(~p, ~p) is not a valid cell in the board\n', [X, Y]), fail.

%check if there is a stack in a cell and, if so, if that stack belongs or can be played by the current player (assumes isValid(X, Y))
checkBelongsToPlayer(X, Y):-
    getBoardTopColor(X, Y, TopColor),
    player(CurrentPlayer),
    getColors(CurrentPlayer, ClaimedColors),
    toClaim(ToClaim),
    append([ToClaim, ClaimedColors], CanMove),%merge the two lists to get the colors the player can move
    nth0(_, CanMove, TopColor). %if the TopColor is one of the colors the player can move
checkBelongsToPlayer(_,_):-
    write('You cannot play this stack because of its top color'), fail.

%checks if the piece can go from xf to yf in a straigh line, without interruption
checkValidMove().%TODO this is where i left of

%prompt for valid X and Y coordinates for origin and destination
getMoveCoordinates(Xf, Yf, Xt, Yt):-
    write('Choose cells FROM and TO using the following format "Xfrom-Yfrom:Xto-Yto."\n'),
    read(Xf-Yf:Xt-Yt),
    checkValidCell(Xf, Yf),
    checkValidCell(Xt, Yt),
    checkBelongsToPlayer(Xf, Yf),
    checkValidMove(Xf, Yf, Xt, Yt).

canMove(Xf, Yf, Xt, Yt):-


