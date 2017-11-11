/*
alphaBeta.pl: predicates for the alphaBeta search algorithm
*/

startAlphaBeta(MaxDepth, Move):-
    % 0 is the current depth, -10000 and 10000 are the starting alpha and beta, 1 is true (means it is starting with a maximizing (alpha) player)
    % alphabeta(0, MaxDepth, -10000, 10000, 1, -10000:Move).
    alphabeta(0, MaxDepth, -10000, 10000, 1, Move).

% alphabeta(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Value):-
alphabeta(Depth, Depth, _Alpha, _Beta, _IsMaximizing, Value):-%depth matched max depth, return the heuristic
    player(Player), %TODO: rewrite evaluateBoard to receive no player
    evaluateBoard(Player, Value). %get the score of the possibility.

%TODO: missing end check for terminal states!!!!!!


alphabeta(Depth, MaxDepth, Alpha, Beta, 1, NewValue):-%is maximizingPlayer (alpha)
    Value is -10000,
    getPossibilities(Possibilities), %get all the children of this node
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 1, Possibilities, Value, NewValue).

alphabeta(Depth, MaxDepth, Alpha, Beta, 0, NewValue):-%is maximizingPlayer (alpha)
    Value is +10000,
    getPossibilities(Possibilities), %get all the children of this node
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 1, Possibilities, Value, NewValue).


%applyAlphabeta(_Depth, _Alpha, _Beta, _IsMaximizing, [], Value, NewValue):-
applyAlphabeta(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, [], Value, Value).
applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 1, [Possibility | Others], Value, FinalValue):-%foreach for max
    pushGame,
        executeBotMove(Possibility), %execute the possibility on the new game instance
        NewDepth is Depth + 1,
        alphabeta(NewDepth, MaxDepth, Alpha, Beta, 0, NewValue), %call the min
        max_member(TempValue, [Value, NewValue]),
        max_member(NewAlpha, [Alpha, TempValue]),
        testExploreNext(Depth, MaxDepth, NewAlpha, Beta, 1, Others, TempValue, FinalValue),
    popGame.

applyAlphabeta(Depth, MaxDepth, Alpha, Beta, 0, [Possibility | Others], Value, FinalValue):-%foreach for min
    pushGame,
        executeBotMove(Possibility), %execute the possibility on the new game instance
        NewDepth is Depth + 1,
        alphabeta(NewDepth, MaxDepth, Alpha, Beta, 1, NewValue), %call the max
        min_member(TempValue, [Value, NewValue]),
        min_member(NewBeta, [Beta, TempValue]),
        testExploreNext(Depth, MaxDepth, Alpha, NewBeta, 1, Others, TempValue, FinalValue),
    popGame.


testExploreNext(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Others, Value, FinalValue):-
    Alpha > Beta,%in this case, proceed
    applyAlphabeta(Depth, MaxDepth, Alpha, Beta, IsMaximizing, Others, Value, FinalValue).
testExploreNext(_Depth, _MaxDepth, _Alpha, _Beta, _IsMaximizing, _Others, Value, Value).%does not explore any further - prunning

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

getPossibilities(Possibilities):-
    getMoveableColorsByPlayer(MoveableColors),
    findall(Xf-Yf-Xt-Yt-Color, getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), Possibilities).
