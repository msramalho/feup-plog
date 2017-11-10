/* findall(X-Y, validMove(J, X, Y, NewBoard), L).
setof(V-X-Y, (validMove(J, X, Y, NewBoard), evaluateBoard(NewBoard, J)), L).
 */

playBotByLevel(random, Move):-
    explorePossibilities(Possibilities),
    random_select(Move, Possibilities, _).

playBotByLevel(basic, Move):-
    write('TODO by level basic'),
    abort.

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