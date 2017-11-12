/*
alphaBeta.pl: predicates for the alphaBeta search algorithm
*/

startAlphaBeta(MaxDepth, Move):-
    % 0 is the current depth, -10000 and 10000 are the starting alpha and beta, 1 is true (means it is starting with a maximizing (alpha) player)
    % alphabeta(0, MaxDepth, -10000, 10000, 1, -10000:Move).
    write('starting a.B...\n'),
    %trace,
    alphabeta(0, MaxDepth, -10000:nothing, 10000:nothing, 1, _, Move).

% alphabeta(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Value):-
alphabeta(Depth, Depth, _Alpha, _Beta, _IsMaximizing, Possibility, Value:Possibility):-%depth matched max depth, return the heuristic
    evaluateBoard(Value), %get the score of the possibility
    invertPlayers.

alphabeta(Depth, MaxDepth, Alpha, Beta, 1, _Possibility, NewValue):-%is maximizingPlayer (alpha)
    currentPlayerHasMoves, !,
    Value = -10000:nothing,
    getPossibilities(Possibilities), %get all the children of this node

    %length(Possibilities, Len), write('MAXIMIZING alphabeta depth is '), write(Depth), write(', a = '), write(Alpha), write(', B = '), write(Beta), write(', Possibilities: '), write(Len), nl,

    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 1, Possibilities, Value, NewValue).

alphabeta(Depth, MaxDepth, Alpha, Beta, 0, _Possibility, NewValue):-%is minimizingPlayer (beta)
    currentPlayerHasMoves, !,
    Value = 10000:nothing,
    getPossibilities(Possibilities), %get all the children of this node

    %length(Possibilities, Len),write('MINIMIZING alphabeta depth is '), write(Depth), write(', a = '), write(Alpha), write(', B = '), write(Beta), write(', Possibilities: '), write(Len), nl,

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
        %write('->Executing: '), write(Possibility),
        NewDepth is Depth + 1,
        alphabeta(NewDepth, MaxDepth, Alpha, Beta, 0, Possibility, NewValue), %call the min
        max_member(TempValue, [Value, NewValue]),
        max_member(NewAlpha, [Alpha, TempValue]),
    popGame,
    testExploreNext(Depth, MaxDepth, NewAlpha, Beta, 1, Others, TempValue, FinalValue).

applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 0, [Possibility | Others], Value, FinalValue):-%foreach for min
    pushGame,
        executeBotMove(Possibility), %execute the possibility on the new game instance
        %write('->Executing: '), write(Possibility), nl,
        NewDepth is Depth + 1,
        alphabeta(NewDepth, MaxDepth, Alpha, Beta, 1, Possibility, NewValue), %call the max
        min_member(TempValue, [Value, NewValue]),
        min_member(NewBeta, [Beta, TempValue]),
    popGame,
    testExploreNext(Depth, MaxDepth, Alpha, NewBeta, 0, Others, TempValue, FinalValue).


testExploreNext(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, [], Value, Value).
testExploreNext(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Others, Value, FinalValue):-
    %format('    testing ~d > ~d \n', [Beta, Alpha]),
    Beta = B:_, Alpha = A:_,
    B > A,%in this case, proceed
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Others, Value, FinalValue).
testExploreNext(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, _Others, Value, Value):-write('p').%:-write('   beta <= alpha  -> prunning\n').%does not explore any further - prunning

getPossibilities(Possibilities):-
    getMoveableColorsByPlayer(MoveableColors),
    findall(Xf-Yf-Xt-Yt-Color, getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), Possibilities).
/*
max_member()

01 function alphabeta(node, depth, α, β, maximizingPlayer)
02      if depth = 0 or node is a terminal node
03          return the heuristic value of node
04      if maximizingPlayer
05          v := -∞
06          for each child of node
07              v := max(v, alphabeta(child, depth – 1, α, β, FALSE))
08              α := max(α, v)
09              if β ≤ α
10                  break (* β cut-off *)
11          return v
12      else
13          v := +∞
14          for each child of node
15              v := min(v, alphabeta(child, depth – 1, α, β, TRUE))
16              β := min(β, v)
17              if β ≤ α
18                  break (* α cut-off *)
19          return v
(* Initial call *)
alphabeta(origin, depth, -∞, +∞, TRUE)
*/

