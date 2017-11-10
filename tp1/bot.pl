/* findall(X-Y, validMove(J, X, Y, NewBoard), L).
setof(V-X-Y, (validMove(J, X, Y, NewBoard), evaluateBoard(NewBoard, J)), L).
 */

playBotByLevel(random, Move):-
    getMoveableColorsByPlayer(MoveableColors),
    findall(Xf-Yf-Xt-Yt-Color, getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), Possibilities),
    random_select(Move, Possibilities, _).

playBotByLevel(greedy, Move):-
    getMoveableColorsByPlayer(MoveableColors),
    setof(Score:Xf-Yf-Xt-Yt-Color, (getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), evaluateMove(Xf-Yf-Xt-Yt-Color, Score)), Possibilities),
    last(Possibilities, Score:Move).

playBotByLevel(Number):-
    integer(Number),
    write('TODO by level for number'),
    abort.

playBot(Bot):-
    botLevel(Bot, Level),
    playBotByLevel(Level, Move),
    write('Bot is executing move: '), write(Move), write(' ... '),
    %sleep(0.5),
    executeBotMove(Move),
    write('...move done\n').


%just move
executeBotMove(Xf-Yf-Xt-Yt-none):-executeMove(Xf, Yf, Xt, Yt).
%claim and then move
executeBotMove(Xf-Yf-Xt-Yt-Color):-claim(Color),executeMove(Xf, Yf, Xt, Yt).

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


evaluateMove(Move, Score):-
    once(pushGame),
        once(player(Player)), % the current player
        once(executeBotMove(Move)), %execute the possibility on the new game instance
        once(evaluateBoard(Player, Score)), %get the score of the possibility
    once(popGame).


%get all the stacks a given stack can be moved to
getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, ClaimedColor):-%with claim
    once(validClaim), %the player has claimed less than 2 colors

    isClaimableColor(ClaimedColor), %is this a valid color to claim
    once(append([MoveableColors, [ClaimedColor]], NewMoveableColors)), %the claimed color can also be moved

    isValid(Xf, Yf),
    once(checkStackNotEmpty(Xf, Yf)),

    isValid(Xt, Yt),
    once(checkStackNotEmpty(Xt, Yt)),

    once(isColorInStackPlayable(Xf, Yf, NewMoveableColors)), %the stack belongs to the player
    once(hasNoDuplicateColors(Xf, Yf, Xt, Yt)),
    once(canPileStacks(Xf, Yf, Xt, Yt)),
    once(checkValidMove(Xf, Yf, Xt, Yt)).

%get all the stacks a given stack can be moved to
getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, none):-%no color claimed
    isValid(Xf, Yf),
    once(checkStackNotEmpty(Xf, Yf)),

    isValid(Xt, Yt),
    once(checkStackNotEmpty(Xt, Yt)),

    isColorInStackPlayable(Xf, Yf, MoveableColors),
    once(hasNoDuplicateColors(Xf, Yf, Xt, Yt)),
    once(canPileStacks(Xf, Yf, Xt, Yt)),
    once(checkValidMove(Xf, Yf, Xt, Yt)).



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