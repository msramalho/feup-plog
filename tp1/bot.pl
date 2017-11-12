/*
bot.pl: all the predicates for validating and choosing the bots' moves
*/
:- include('alphaBeta.pl').

playBotByLevel(random, Move):-%random move
    getMoveableColorsByPlayer(MoveableColors),
    findall(Xf-Yf-Xt-Yt-Color, getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), Possibilities),
    random_select(Move, Possibilities, _).

playBotByLevel(greedy, Move):-%greedy move
    getMoveableColorsByPlayer(MoveableColors),
    setof(Score:Xf-Yf-Xt-Yt-Color, (getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, Color), evaluateMove(Xf-Yf-Xt-Yt-Color, Score)), Possibilities),
    last(Possibilities, _Score:Move).

playBotByLevel(Number, Move):-%hardcore move (alpha-beta)
    integer(Number),
    startAlphaBeta(Number, _Value:Move).

playBot(Bot):-
    botLevel(Bot, Level),
    playBotByLevel(Level, Move),
    write('Bot is executing move: '), write(Move), nl,
    executeBotMove(Move).

%--------------------------------------------get valid moves

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

%get all the stacks a given stack can be moved to
getFullValidMove(MoveableColors, Xf, Yf, Xt, Yt, ClaimedColor):-%with claim
    once(validClaim), %the player has claimed less than 2 colors

    isClaimableColor(ClaimedColor), %is this a valid color to claim
    once(append([MoveableColors, [ClaimedColor]], NewMoveableColors)), %the claimed color can also be moved

    getFullValidMove(NewMoveableColors, Xf, Yf, Xt, Yt, none).


executeBotMove(Xf-Yf-Xt-Yt-none):-move(Xf, Yf, Xt, Yt).%just move
executeBotMove(Xf-Yf-Xt-Yt-Color):-claim(Color),move(Xf, Yf, Xt, Yt).%claim and then move

%--------------------------------------------player board score
%evaluate the current board according to the current player, higher Score means better game for Player
evaluateBoard(Score):-player(Player),evaluateBoard(Player, Score).
evaluateBoard(Player, Score):-
    getColors(Player, Colors),
    getPlayerStackScore(Player, Colors, 5, Score).

getPlayerStackScore(Player, Colors, Height, Score):-  %reached the end, return the score of the stacks
    Height =< 0,
    getStacks(Player, Stacks),
    length(Stacks, TempScore), %count the height of the stacks
    length(Colors, LenColors), %count the number of claimed colors
    Score is ((TempScore * 20) + ((2- LenColors) * 10)). %each stack is worth 10 points, each unclaimed color 5

getPlayerStackScore(Player, Colors, Height, StackScore):- %if there are still heights to evaluate
    countStacksByColorAndHeight(Colors, Height, Count),
    NewHeight is Height - 1,
    getPlayerStackScore(Player, Colors, NewHeight, TempScore),
    StackScore is ((Count * Height) + TempScore). %each extra stack is worth it's height (1 to 4)

evaluateMove(Move, Score):-
    once(pushGame),
        once(executeBotMove(Move)), %execute the possibility on the new game instance
        once(evaluateBoard(Score)), %get the score of the possibility
    once(popGame).

%--------------------------------------------bot difficulty
validBotLevel(r, random).%random move
validBotLevel(g, greedy).%greedy move
validBotLevel(Number, Number):-%numeric value (alfa-beta prunning)
    integer(Number), Number > 0.%random move

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