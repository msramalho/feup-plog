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
    sleep(0.5),
    executeBotMove(Move).


%claim and then move
executeBotMove(Xf-Yf-Xt-Yt-Color):-claim(Color),executeBotMove(Xf-Yf-Xt-Yt).
%just move
executeBotMove(Xf-Yf-Xt-Yt):-executeMove(Xf, Yf, Xt, Yt).

/*
%evaluate the current board according to the current player
evaluateBoard:-player(Player),evaluateBoard(Player).
%evaluate the current board according to a given player
evaluateBoard(Player).*/






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