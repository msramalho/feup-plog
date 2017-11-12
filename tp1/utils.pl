/*
utils.pl: file with the useful, contextless and reused predicates.
*/
setRandomSeed:-
	now(Time), S is Time mod 30269,
	getrand(random(X, Y, Z, _)),
	setrand(random(S, X, Y, Z)), !.
:-setRandomSeed. %set a random seed

listToString(List, Result):-
	listToString(List, '', Result).

listToString([], Final, Final).
listToString([H|T], Temp, Final):-
	atom_concat(Temp, H, NewRes),
	listToString(T, NewRes, Final).

numberToString(Number, String):-
	number_chars(Number, TempList),
	listToString(TempList, String).

writeString(_, 0).
writeString(String, Count):-
    write(String),
    NewCount is Count - 1,
    writeString(String, NewCount).

%replace a stack at the Line, Column by the NewStack
replaceBoardStack(Board, X, Y, NewStack, NewBoard):-
    nth0(X, Board, OldLine, BoardWithoutLine), %remove the line from the board
    nth0(Y, OldLine, _OldStack, LineWithoutStack), %remove the old stack from te line
    nth0(Y, NewLine, NewStack, LineWithoutStack), %add the stack to the line
    nth0(X, NewBoard, NewLine, BoardWithoutLine). %add the new line to the board


getBoardTopColor(X, Y, TopColor):-
    board(B),
    nth0(X, B, Line), %get the line
    nth0(Y, Line, [TopColor|_]). %get the cell and the head which is the top color

getBoardStack(X, Y, Stack):-
    board(B),
    nth0(X, B, Line),  %get the line
    nth0(Y, Line, Stack). %get the Stack

%get the stack height
getBoardStackHeight(X, Y, Height):-
    getBoardStack(X, Y, Stack),
    length(Stack, Height).

%get the player that owns this color, fails if none:
getPlayerFromColor(Color, _Player):-
    toClaim(ToClaim),
    nth0(_, ToClaim, Color), %if the color is not claimed, fails
    !, fail.
%assuming then, that this color is claimed, it belongs to player1, player2 or bot
%testing player1
getPlayerFromColor(Color, Player):-testPlayerOwnsColor(player1, Color, Player).
getPlayerFromColor(Color, Player):-testPlayerOwnsColor(player2, Color, Player).
getPlayerFromColor(Color, Player):-testPlayerOwnsColor(bot, Color, Player).
getPlayerFromColor(Color, Player):-testPlayerOwnsColor(bot1, Color, Player).
getPlayerFromColor(Color, Player):-testPlayerOwnsColor(bot2, Color, Player).

testPlayerOwnsColor(Player, Color, Player):-
    getColors(Player, Colors),
    nth0(_, Colors, Color). %if the color belongs to the player

clear:-write('\33\[2J').

%---------------------------------------------------------------------------
% databse manipulation helpers, remove arity one for consulting the database
% board database helpers
board(B):-
    game(G),
    board(G, B).
saveBoard(B):-
    game(G),
    tryRetract(board(G, _)),
    assert(board(G, B)).
% player database helpers
player(Player):-
    game(G),
    player(G, Player).
savePlayer(Player):-
    game(G),
    tryRetract(player(G, _)),
    assert(player(G, Player)).
% next player database helpers
nextPlayer(Player):-
    game(G),
    nextPlayer(G, Player).
saveNextPlayer(Player):-
    game(G),
    tryRetract(nextPlayer(G, _)),
    assert(nextPlayer(G, Player)).
% to claim database helpers
toClaim(Colors):-
    game(G),
    toClaim(G, Colors).
saveToClaim(Colors):-
    game(G),
    tryRetract(toClaim(G, _)),
    assert(toClaim(G, Colors)).
% get colors database helpers
getColors(Player, Colors):-
    game(G),
    getColors(G, Player, Colors).
getColors(Colors):-%default gets for the current player
    player(Player),
    game(G),
    getColors(G, Player, Colors).
saveGetColors(Player, Colors):-
    game(G),
    tryRetract(getColors(G, Player, _)),
    assert(getColors(G, Player, Colors)).
saveGetColors(Colors):-%default saves for the current player
    player(Player),
    saveGetColors(Player, Colors).
% get stacks database helpers
getStacks(Player, Stacks):-
    game(G),
    getStacks(G, Player, Stacks).
getStacks(Stacks):-%default gets for the current player
    player(Player),
    game(G),
    getStacks(G, Player, Stacks).
saveGetStacks(Player, Stacks):-
    game(G),
    tryRetract(getStacks(G, Player, _)),
    assert(getStacks(G, Player, Stacks)).
saveGetStacks(Stacks):-%default saves for the current player
    player(Player),
    saveGetStacks(Player, Stacks).
% has claimed database helpers
hasClaimed(Val):-
    game(G),
    hasClaimed(G, Val).
saveHasClaimed(Val):-
    game(G),
    tryRetract(hasClaimed(G, _)),
    assert(hasClaimed(G, Val)).
clearHasClaimed:-
    abolish(hasClaimed/2),
    saveHasClaimed(false).
% bot level database helpers
botLevel(Bot, Level):-
    game(G),
    botLevel(G, Bot, Level).
saveBotLevel(Bot, Level):-
    game(G),
    tryRetract(botLevel(G, Bot, _)),
    assert(botLevel(G, Bot, Level)).

% switch game, can move from original to temporary and such
saveGame(NewGame):-
    tryRetract(game(_)),
    assert(game(NewGame)).

%duplicate a game state so a new one can be tested and destroyed at will
duplicateGame(Original, Duplicate):-
    %duplicate the board
    board(Original, B),
    assert(board(Duplicate, B)),
    %duplicate the player
    player(Original, P),
    assert(player(Duplicate, P)),
    %duplicate the next player
    nextPlayer(Original, NP),
    assert(nextPlayer(Duplicate, NP)),
    %duplicate the toClaim
    toClaim(Original, TC),
    assert(toClaim(Duplicate, TC)),
    %duplicate the getColors for player
    getColors(Original, P, CP),
    assert(getColors(Duplicate, P, CP)),
    %duplicate the getColors for the next player
    getColors(Original, NP, CNP),
    assert(getColors(Duplicate, NP, CNP)),
    %duplicate the getStacks for player
    getStacks(Original, P, SP),
    assert(getStacks(Duplicate, P, SP)),
    %duplicate the getStacks for the next player
    getStacks(Original, NP, SNP),
    assert(getStacks(Duplicate, NP, SNP)),
    % hasClaimed starts as false
    %hasClaimed(Original, _H),
    assert(hasClaimed(Duplicate, false)).

destroyGame(ToDestroy):-
    %get this game's players
    player(ToDestroy, P),
    nextPlayer(ToDestroy, NP),

    %detroy all its related values, if they exist
    tryRetract(game(ToDestroy)),
    tryRetract(board(ToDestroy, _)),
    tryRetract(player(ToDestroy, _)),
    tryRetract(nextPlayer(ToDestroy, _)),
    tryRetract(toClaim(ToDestroy, _)),
    tryRetract(getColors(ToDestroy, P, _)),
    tryRetract(getColors(ToDestroy, NP, _)),
    tryRetract(getStacks(ToDestroy, P, _)),
    tryRetract(getStacks(ToDestroy, NP, _)),
    tryRetract(hasClaimed(ToDestroy, _)).

%does not fail if retract fails
tryRetract(ToRetract):-retract(ToRetract).
tryRetract(_).

% push a new game and make this the valid one, increments game index by 1
pushGame:-
    game(Current), %get the current game
    NewGame is Current + 1,
    duplicateGame(Current, NewGame),
    saveGame(NewGame).

%return to a previous game state
popGame:-
    game(Current), %get the current game
    PreviousGame is Current - 1,
    PreviousGame >= 0,
    destroyGame(Current),
    saveGame(PreviousGame).

