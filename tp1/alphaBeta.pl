/*
alphaBeta.pl: predicates for the alphaBeta search algorithm
*/

startAlphaBeta(MaxDepth, Move):-
    % 0 is the current depth, -10000 and 10000 are the starting alpha and beta, 1 is true (means it is starting with a maximizing (alpha) player)
    write('starting a.B...\n'),
    alphabeta(0, MaxDepth, -10000:nothing, 10000:nothing, 1, _, Move).

% alphabeta(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Value):-
alphabeta(Depth, Depth, _Alpha, _Beta, _IsMaximizing, Possibility, Value:Possibility):-%depth matched max depth, return the heuristic
    evaluateBoard(Value), %get the score of the possibility
    invertPlayers.

alphabeta(Depth, MaxDepth, Alpha, Beta, 1, _Possibility, NewValue):-%is maximizingPlayer (alpha)
    currentPlayerHasMoves, !,
    Value = -10000:nothing,
    getPossibilities(Possibilities), %get all the children of this node
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 1, Possibilities, Value, NewValue).

alphabeta(Depth, MaxDepth, Alpha, Beta, 0, _Possibility, NewValue):-%is minimizingPlayer (beta)
    currentPlayerHasMoves, !,
    Value = 10000:nothing,
    getPossibilities(Possibilities), %get all the children of this node
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 0, Possibilities, Value, NewValue).

alphabeta(Depth, MaxDepth, Alpha, Beta, 1, Possibility, NewValue):-%MAX, current player has no moves, check if next player has
    invertPlayers,
    currentPlayerHasMoves, !,
    NewDepth is Depth + 1,
    alphabeta(NewDepth, MaxDepth, Alpha, Beta, 0, Possibility, NewValue).

alphabeta(Depth, MaxDepth, Alpha, Beta, 0, Possibility, NewValue):-%MIN, current player has no moves, check if next player has
    invertPlayers,
    currentPlayerHasMoves, !,
    NewDepth is Depth + 1,
    alphabeta(NewDepth, MaxDepth, Alpha, Beta, 1, Possibility, NewValue).

alphabeta(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, Possibility, Value:Possibility):-%this is a terminal state
    evaluateBoard(Value).

%applyAlphabeta(_Depth, _Alpha, _Beta, _IsMaximizing, [], Value, NewValue):-
applyAlphabeta(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, [], Value, Value).
applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 1, [Possibility | Others], Value, FinalValue):-%foreach for max
    pushGame,
        executeBotMove(Possibility), %execute the possibility on the new game instance
        NewDepth is Depth + 1,
        alphabeta(NewDepth, MaxDepth, Alpha, Beta, 0, Possibility, NewValue), %call the min
        max_member(TempValue, [Value, NewValue]),
        max_member(NewAlpha, [Alpha, TempValue]),
    popGame,
    testExploreNext(Depth, MaxDepth, NewAlpha, Beta, 1, Others, TempValue, FinalValue).

applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 0, [Possibility | Others], Value, FinalValue):-%foreach for min
    pushGame,
        executeBotMove(Possibility), %execute the possibility on the new game instance
        NewDepth is Depth + 1,
        alphabeta(NewDepth, MaxDepth, Alpha, Beta, 1, Possibility, NewValue), %call the max
        min_member(TempValue, [Value, NewValue]),
        min_member(NewBeta, [Beta, TempValue]),
    popGame,
    testExploreNext(Depth, MaxDepth, Alpha, NewBeta, 0, Others, TempValue, FinalValue).


testExploreNext(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, [], Value, Value).
testExploreNext(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Others, Value, FinalValue):-
    Beta = B:_, Alpha = A:_,
    B > A,%in this case, proceed
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Others, Value, FinalValue).
testExploreNext(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, _Others, Value, Value):-write('p').

getPossibilities(Possibilities):-
    getMoveableColorsByPlayer(MoveableColors),
    findall(Xf-Yf-Xt-Yt-Color, getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), Possibilities).