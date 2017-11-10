/* findall(X-Y, validMove(J, X, Y, NewBoard), L).
setof(V-X-Y, (validMove(J, X, Y, NewBoard), evaluateBoard(NewBoard, J)), L).
 */

negativeInf(-2^2147483616).
positiveInf(2^2147483616).

playBotByLevel(random, Move):-
    explorePossibilities(Possibilities),
    random_select(Move, Possibilities, _).

playBotByLevel(basic, Move):-
    explorePossibilities([First| OtherPossibilities]),
    negativeInf(NInf),
    basicLevelSelect(OtherPossibilities, First, NInf, Move, _). %bests score starts as -inf

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


%---------------bot basic move
getBest(_CurrentMove, Possibility, CurrentBest, Score, NewMove, NewBest):- %new possibility is better
    CurrentBest < Score, NewMove = Possibility, NewBest = CurrentBest.
getBest(CurrentMove, _Possibility, CurrentBest, _Score, CurrentMove, CurrentBest). %new possibility is worse

basicLevelSelect([], FinalMove, FinalBest, FinalMove, FinalBest).
basicLevelSelect([Possibility | Others], CurrentMove, CurrentBest, FinalMove, FinalBest):-
    write('pushing...'),
    % TODO: try to repeat the experience but by passing a board and the other variables instead of a push and pop
    pushGame,
        player(Player), % the current player
        once(executeBotMove(Possibility)), %execute the possibility on the new game instance
        once(evaluateBoard(Player, Score)), %get the score of the possibility
        write('(move '), write(Possibility), write(' leads to score of '), write(Score), write(')'),
    popGame,
    write('...done'), nl,
    %choose the best move according to the pontuation
    getBest(CurrentMove, Possibility, CurrentBest, Score, NewMove, NewBest),
    basicLevelSelect(Others, NewMove, NewBest, FinalMove, FinalBest).

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
validBotLevel(b, basic).%basic move
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