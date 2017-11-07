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


%iterate over a list of X-Y-Color and give those to the correct player
moveStackToPLayer([]).
moveStackToPLayer([X-Y-Stack|T]):-
    Stack = [TopColor|_],
    getPlayerFromColor(TopColor, Player), %if this fails the color does not belong to any player
    %update the player's stacks with the new one
    getStacks(Player, PLayerStacks),
    append([PLayerStacks, [Stack]], NewStacks),
    saveGetStacks(Player, NewStacks),
    %remove the stack from the board
    board(B),
    replaceBoardStack(B, X, Y, [], NewBoard),
    saveBoard(NewBoard),
    moveStackToPLayer(T).
%simply call itself again ignoring this one, to get here it means this stack is an obstacle (belongs to no one)
moveStackToPLayer([_|T]):-
    moveStackToPLayer(T).

removeClaimedStacksWithFive:-
    findall(X-Y-Stack, (getBoardStackHeight(X, Y, 5), getBoardStack(X, Y, Stack)), L), %get all the blocks with height 5
    remove_dups(L, Pruned), %remove duplicates
    moveStackToPLayer(Pruned).

%fails if current player has no move left
currentPlayerHasMoves:-
    explorePossibilities(Possibilities),
    length(Possibilities, Len),
    Len > 0.

%check if the current player has moves, if not check if the next player has moves, if not endGame
evaluateBoard:-currentPlayerHasMoves, !.
evaluateBoard:-%current player has no moves, invert and test
    invertPlayers,
    currentPlayerHasMoves.
evaluateBoard:-%if no player has moves, end the game
    write('no more valid moves'), nl, exit.

%fails if X and Y's top color is not a neutral stack
isStackNeutral(X, Y):-
    getBoardTopColor(X, Y, Piece),
    toClaim(NeutralPieces),
    nth0(_, NeutralPieces, Piece). %if this is a neutral stack
%fails if X and Y's top color does not belong to the current player
isStackOfPlayer(X, Y):-
    player(CurentPlayer),
    getBoardTopColor(X, Y, Piece),
    getColors(CurentPlayer, ClaimedColors),
    nth0(_, ClaimedColors, Piece). %if this is the player's stack


%---------------------------------- Evaluate the state of the game
%Assertion functions to get all the possible moves

isStackMoveValid(_, [], []).
isStackMoveValid(MoveableColors, [X-Y |T], NewResult):-
    findall(X-Y-Xt-Yt, isMoveValid(MoveableColors, X, Y, Xt, Yt), AllMoves),
    %remove_dups(AllMoves, AllMovesPruned),
    isStackMoveValid(MoveableColors, T, Result),
    append([Result, AllMoves], NewResult).

%get all the stacks a given stack can be moved to
isMoveValid(MoveableColors, Xf, Yf, Xt, Yt):-
    once(isColorInStackPlayable(Xf, Yf, MoveableColors)),
    isValid(Xt, Yt),
    once(checkStackNotEmpty(Xt, Yt)),
    once(checkValidMove(Xf, Yf, Xt, Yt)),
    once(hasNoDuplicateColors(Xf, Yf, Xt, Yt)),
    once(canPileStacks(Xf, Yf, Xt, Yt)).
    /*,
    isNeutralStackJumpTo(Xf, Yf, Xt, Yt).*/

%get all the moves the currentPlayer can do
getPlayerMoves(AllMoves):-
    getMoveableColorsByPlayer(MoveableColors), %merge the two lists to get the colors the player can move
    findall(X-Y, isColorInStackPlayable(X, Y, MoveableColors), Moves),
    %write('player can play:'), write(Moves), nl,
    isStackMoveValid(MoveableColors, Moves, AllMoves).
    %remove_dups(AllMoves, AllMovesPruned),
    %write('all possible moves are:'), write(AllMoves), nl.

%helper function for exploreClaims, appends the color at the end
addColorAtEnd([], _, []).
addColorAtEnd([Move | R], Color, [Move-Color | NewRest]):-
    addColorAtEnd(R, Color, NewRest).


%make a claim, if possible and then getAllPossibilities
exploreClaims([], Final, Final).
exploreClaims([Color | R], Temp, NewRes):-
    validClaim,
    pushGame,
        claim(Color),
        getPlayerMoves(AllMoves),
        addColorAtEnd(AllMoves, Color, AllMovesReady),
    popGame,
    append([Temp, AllMovesReady], Res),
    exploreClaims(R, Res, NewRes).
exploreClaims([_|R], Temp, Res):-
    exploreClaims(R, Temp, Res).


%get all the next plays possible actions in a list of Xf-Yf:Xt-Yt:claim where claim can be a color to claim or none
explorePossibilities(Possibilities):-
    getPlayerMoves(AllMoves),
    toClaim(ToClaim),
    exploreClaims(ToClaim, AllMoves, Possibilities).
    %write('all possible moves are:'), write(Possibilities), nl.


