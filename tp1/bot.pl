/* findall(X-Y, validMove(J, X, Y, NewBoard), L).
setof(V-X-Y, (validMove(J, X, Y, NewBoard), evaluateBoard(NewBoard, J)), L).
 */

playBotByLevel(random, Move):-
    explorePossibilities(Possibilities),
    random_select(Move, Possibilities, _).

playBotByLevel(greedy, Move):-
    explorePossibilities([First| OtherPossibilities]),
    greedyLevelSelect(OtherPossibilities, First, -1000, Move, _). %bests score starts as -inf

playBotByLevel(Number):-
    integer(Number),
    write('TODO by level for number'),
    abort.

playBot(Bot):-
    botLevel(Bot, Level),
    playBotByLevel(Level, Move),
    write('Bot is executing move: '), write(Move), write(' ... '), nl,
    %sleep(0.5),
    executeBotMove(Move).


%claim and then move
executeBotMove(Xf-Yf-Xt-Yt-Color):-claim(Color),executeBotMove(Xf-Yf-Xt-Yt).
%just move
executeBotMove(Xf-Yf-Xt-Yt):-executeMove(Xf, Yf, Xt, Yt).

%CurrentMove = cmove, NewMove = move, CurrentScore = 10, NewScore = 20, getBest(CurrentMove, NewMove, CurrentScore, NewScore, FinalMove, FinalScore).
%---------------bot greedy move
getBest(_CurrentMove, NewMove, CurrentScore, NewScore, NewMove, NewScore):- %new possibility is better
    %write('\n    IS '), write(CurrentScore),  write(' < '), write(NewScore), write(' ? \n'),
    CurrentScore < NewScore.
getBest(CurrentMove, _NewMove, CurrentScore, _NewScore, CurrentMove, CurrentScore). %new possibility is worse

greedyLevelSelect([], FinalMove, FinalScore, FinalMove, FinalScore).
greedyLevelSelect([Possibility | Others], CurrentMove, CurrentScore, FinalMove, FinalScore):-
    %write('pushing...'),
    % TODO: try to repeat the experience but by passing a board and the other variables instead of a push and pop
    pushGame,
        player(Player), % the current player
        executeBotMove(Possibility), %execute the possibility on the new game instance
        evaluateBoard(Player, Score), %get the score of the possibility
        %write('(move '), write(Possibility), write(' leads to score of '), write(Score), write(')'),
    popGame,
    %write('...done'), nl,
    %choose the best move according to the pontuation
    getBest(CurrentMove, Possibility, CurrentScore, Score, NewMove, NewBest),
    greedyLevelSelect(Others, NewMove, NewBest, FinalMove, FinalScore).

%---------------player board score
%evaluate the current board according to the current player, higher Score means better game for Player
evaluateBoard(Player, Score):-
    getColors(Player, Colors),
    getPlayerStackScore(Player, Colors, 5, Score). %, write('Player '), write(Player), write(' has a score of: '), write(Score).

getPlayerStackScore(Player, Colors, Height, Score):-  %reached the end, return the score of the stacks
    Height =< 0,
    getStacks(Player, Stacks),
    length(Stacks, TempScore), %count the height of the stacks
    length(Colors, LenColors), %count the number of claimed colors
    Score is TempScore * 10 + LenColors + 5. %each stack is worth 10 points, each unclaimed color 5

getPlayerStackScore(Player, Colors, Height, StackScore):- %if there are still heights to evaluate
    countStacksByColorAndHeight(Colors, Height, Count),
    NewHeight is Height -1,
    getPlayerStackScore(Player, Colors, NewHeight, TempScore),
    StackScore is ((Count * Height) + TempScore). %each extra stack is worth it's height (1 to 4)








%----------bot difficulty
validBotLevel(r, random).%random move
validBotLevel(g, greedy).%greedy move
validBotLevel(Number, Number):-%numeric value
    integer(Number).%random move

chooseBotLevel(Bot):-
    displayBotLevels(Bot),
    read(BotLevel),
    read_line([]),
    validBotLevel(BotLevel, TranslatedLevel),
    saveBotLevel(Bot, TranslatedLevel).
chooseBotLevel(Bot):-write('Invalid bot difficulty, try again:'), !, chooseBotLevel(Bot).


isBot(bot).
isBot(bot1).
isBot(bot2).