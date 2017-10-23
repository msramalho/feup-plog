/* This file contains all the predicates required for the game logic to be implemented (allowed moves, ...)*/

/* Which board cells can be used (since it is not a board of squares) */

/*[0, 0, 1, 0, 1, 0, 1, 0, 0]*/
/* first line of matrix */
isValid(0, 2).
isValid(0, 4).
isValid(0, 6).
/* second line of matrix */
isValid(1, 3).
isValid(1, 5).
/* third line of matrix */
isValid(2, 2).
isValid(2, 4).
isValid(2, 6).

/*[0, 1, 0, 1, 0, 1, 0, 1, 0]*/
/* fourth line of matrix */
isValid(3, 1).
isValid(3, 3).
isValid(3, 5).
isValid(3, 7).
/* fifth line of matrix */
isValid(4, 2).
isValid(4, 4).
isValid(4, 6).
/* sixth line of matrix */
isValid(5, 1).
isValid(5, 3).
isValid(5, 5).
isValid(5, 7).

/*[1, 0, 1, 0, 1, 0, 1, 0, 1]*/
/* seventh line of matrix */
isValid(6, 0).
isValid(6, 2).
isValid(6, 4).
isValid(6, 6).
isValid(6, 8).

/*[0, 1, 0, 1, 0, 1, 0, 1, 0]*/
/* eigth line of matrix */
isValid(7, 1).
isValid(7, 3).
isValid(7, 5).
isValid(7, 7).
/* nineth line of matrix */
isValid(8, 2).
isValid(8, 4).
isValid(8, 6).
/* tenth line of matrix */
isValid(9, 1).
isValid(9, 3).
isValid(9, 5).
isValid(9, 7).

/*[0, 0, 1, 0, 1, 0, 1, 0, 0]*/
/* eleventh line of matrix */
isValid(10, 2).
isValid(10, 4).
isValid(10, 6).
/* twelfth line of matrix */
isValid(11, 3).
isValid(11, 5).
/* thirteenth line of matrix */
isValid(12, 2).
isValid(12, 4).
isValid(12, 6).


/* convert the isValid predicate into a list of two lists containg the x and the y values of isValid*/
getValidPositions(Result):-
    findall(X-Y, isValid(X,Y), Result).

isMoveValid(Xf, Yf, Xt, Yt):-
    checkValidMove(Xf, Yf),
    checkValidMove(Xt, Yt).

% check if cell is valid (x, y) and print error if not
checkValidMove(X, Y):-isValid(X, Y), !.
checkValidMove(X, Y):-format('Cell(~p, ~p) is not a valid cell in the board\n', [X, Y]), fail.

getMoveCoordinates(Xf, Yf, Xt, Yt):-
    write('choose start and end coordinates in the following format "Xfrom-Yfrom:Xto-Yto."\n'),
    read(Xf-Yf:Xt-Yt),
    format('from ~d,~d to ~d,~d\n', [Xf, Yf, Xt, Yt]),
    checkValidMove(Xf, Yf),
    checkValidMove(Xt, Yt).

updateClaimedColor(Translated):-
    toClaim(ToClaim), %load the colors left for claiming
	write('\nAvailable Colors:'), write(ToClaim), nl,
    nth0(_, ToClaim, Translated, NewToClaim),%if the chosen color is inside ToClaim
    retract(toClaim(_)),
    assert(toClaim(NewToClaim)), %updated available colors
    player(CurrentPlayer).%get the current player
updateClaimedColor(_):-%if update fails then it's because:
    write('This color is no longer available\n'), fail.

readValidColor(Translated):-
    write('Which color would you like to claim?\n'),
    read_line(Color),
    translateColor(Color, Translated),
    updateClaimedColor(Translated).
readValidColor(_):-%if readValid fails it's because:
    write('Invalid color name.\n'), fail.

claimColor:-
    player(CurrentPlayer),%load the current player
    getColors(CurrentPlayer, ChosenColors), %get this player's colors
    length(ChosenColors, Len), %get the length of this player's colors
    Len < 2, !, %if the length is less than 2, then read color else go to other option for claimColor
    repeat,%repeatedly read color
        readValidColor(Translated),
    !,
    append([ChosenColors, [Translated]], Result),
    write('new colors for '), write(CurrentPlayer), write(' are: '), write(Result),nl,
    retract(getColors(CurrentPlayer, _)),
    assert(getColors(CurrentPlayer, Result)),
    assert(hasClaimed).%set the flag hasClaimed to true
    %displayBoard.

claimColor:-
    write('You can only claim two colors\n').

%check if the board is in a final state, probably use validMove and find_all
evaluateBoard:-
    write('----TODO: evaluate board state\n').
