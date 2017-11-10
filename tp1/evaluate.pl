
/*
evaluate.pl: This file implements the predicates required for assessing what are valid positions
*/

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
    getMoveableColorsByPlayer(MoveableColors),
    getFullValidMove(MoveableColors, _Xf, _Yf, _Xt, _Yt, _Color).

%check if the current player has moves, if not check if the next player has moves, if not endGame
assertBoard:-currentPlayerHasMoves.
assertBoard:-%current player has no moves, invert and test
    invertPlayers,
    currentPlayerHasMoves.
assertBoard:-%if no player has moves, end the game
    displayBoard,
    write('no more valid moves'), nl,
    getWinner(Winner),
    displayWinner(Winner),
    exit.

%--------------------- identify te winner start
getWinner(Winner):-%if the one player has more collected stacks than the other
    %get the players
    player(Player), nextPlayer(NextPlayer),
    %get their stacks
    getStacks(Player, StacksP), getStacks(NextPlayer, StacksNP),
    %count the height of the stacks
    length(StacksP, ScoreP), length(StacksNP, ScoreNP),
    decideWiner(Player, NextPlayer, ScoreP, ScoreNP, Winner).%chooses the one with the heighest, fails if none

getWinner(Winner):-%players had stacks with the same height
    player(Player),
    nextPlayer(NextPlayer),
    getWinnerByHeight(Player, NextPlayer, 4, Winner).

getWinner(draw).%if both predicates above fail, then there is a draw

%compare both players in a given height, returning the winner and searching recursively
getWinnerByHeight(Player, NextPlayer, Height, Winner):-
    getColors(Player, ColorsP),
    getColors(NextPlayer, ColorsNP),
    countStacksByColorAndHeight(ColorsP, Height, CountP),
    countStacksByColorAndHeight(ColorsNP, Height, CountNP),
    decideNowOrNext(Player, NextPlayer, CountP, CountNP, Height, Winner).


%try to decide the winner of this height, if it fails, calls comparePlayersHeights with the upcoming height
decideNowOrNext(Player, NextPlayer, CountP, CountNP, _Height, Winner):-%if a winner exists in the current height
    decideWiner(Player, NextPlayer, CountP, CountNP, Winner).

decideNowOrNext(Player, NextPlayer, _CountP, _CountNP, Height, Winner):-%else check the next height (lower)
    NewHeight is Height - 1,
    NewHeight > 0, % if the height is not larger than zero than there is a draw, this predicate fails
    getWinnerByHeight(Player, NextPlayer, NewHeight, Winner). % check the immediatelly lower height

%compare the current heights (or just some player associated score) and return the greatest
decideWiner(Player, _NextPlayer, CountP, CountNP, Winner):-CountP > CountNP, Winner = Player.
decideWiner(_Player, NextPlayer, CountP, CountNP, Winner):-CountNP > CountP, Winner = NextPlayer.

%count how many stacks with this topcolor and this height are in the board
countStacksByColorAndHeight([], _, 0).
countStacksByColorAndHeight([FinalColor], Height, FinalCount):-%the last color just returns
    findall(X-Y, (getBoardStackHeight(X, Y, Height), getBoardTopColor(X, Y, FinalColor)), L), %get all the stacks with color Color and height Height
    length(L, FinalCount).
countStacksByColorAndHeight([Color | OtherColors], Height, FinalCount):-%max claimed colors is two
    countStacksByColorAndHeight(OtherColors, Height, Count),
    findall(X-Y, (getBoardStackHeight(X, Y, Height), getBoardTopColor(X, Y, Color)), L), %get all the stacks with color Color and height Height
    length(L, TempCount),
    FinalCount is Count + TempCount.
%--------------------- identify te winner end


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