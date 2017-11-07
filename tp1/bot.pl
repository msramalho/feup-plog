/* findall(X-Y, validMove(J, X, Y, NewBoard), L).
setof(V-X-Y, (validMove(J, X, Y, NewBoard), evaluateBoard(NewBoard, J)), L).
 */
playBot:-
    explorePossibilities(Possibilities),
    random_select(Move, Possibilities, _),
    write('Bot is executing move: '), write(Move), write(' ... '), nl,
    sleep(0.2),
    executeBotMove(Move).

%claim and then move
executeBotMove(Xf-Yf-Xt-Yt-Color):-claim(Color),executeBotMove(Xf-Yf-Xt-Yt).
%just move
executeBotMove(Xf-Yf-Xt-Yt):-executeMove(Xf, Yf, Xt, Yt).

isBot(bot).
isBot(bot1).
isBot(bot2).