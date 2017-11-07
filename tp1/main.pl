:- include('utils.pl').

:- include('getRandomBoard.pl').
:- include('displayBoard.pl').

:- include('evaluate.pl').
:- include('claim.pl').
:- include('move.pl').
:- include('interface.pl').

%:- include('tests.pl').


% volatile and dynamic predicates declaration
% all the predicates have the first Variable as being the current game
:- volatile
    game/1,         % the current game to be used, numbers starting in 0
    board/2,        % the current board state
    player/2,       % the current player
    nextPlayer/2,   % the next player
    toClaim/2,      % which colors are yet to claim
    getColors/3,    % a list of the colors claimed by the selected player getColors(player, Colors)
    getStacks/3.    % a list of the stacks collected by the selected player getStacks(player, Stacks)
    hasClaimed/2.   % a flag to indicate wheter the current player has claimed a color in this turn

:- dynamic
    game/1,
    board/2,
    player/2,
    nextPlayer/2,
    toClaim/2,
    getColors/3,
    getStacks/3,
    hasClaimed/2.

%make bot start first or human start first, 50% chance
randomizeBotPlay:-
    random_select(Temp, [0, 1], _),
    Temp =:= 0,
    savePlayer(player1),
    saveNextPlayer(bot),
    write('Player 1 goes first...\n').
randomizeBotPlay:-
    savePlayer(bot),
    saveNextPlayer(player1),
    write('Bot goes first...\n').

%game type (User x User | User x Bot)
startGame(quit):-exit. %abort
startGame(instructions):-displayInstructions. %abort
startGame(humanVhuman):- %intialize both players. The real players should randomly choose their turn
    savePlayer(player1),
    saveNextPlayer(player2),
    write('Human Vs Human Selected\n').

startGame(humanVbot):- % initialize the player and the nextPlayer randomly, the bot may be first
    write('Human Vs Bot Selected\n'),
    randomizeBotPlay.

%try to read a valid game type (1(humanVhuman), 2(humanVbot) or 3 (quit))
getGameType(GameType):-
    read_line([GameTypeLine|_]),
    menuTranslate(GameType, GameTypeLine).
getGameType(GameType):-
    write('Wrong game type, try again:\n'),
    getGameType(GameType).

%wait for instruction and enter
waitForInstruction:-
    write('Enter your instruction (move, claim, quit)\n'),
    read_line(Instruction),
    parseInstruction(Instruction).

%expecting a move instruction
parseInstruction("move"):-
    move.

%expecting a claim instruction
parseInstruction("claim"):-%instruction was claim and has not claimed any piece
    !, claimColor. % the players can move after a claim

%ignore empty input->newline
parseInstruction([]):- !, fail.
%unexpected instruction
parseInstruction(_):-
    write('Instruction not recognized, try again.\n'), fail.

%check the bot should play
isBotPlaying:-
    player(bot),!, %the next player is the bot
    playBot.
isBotPlaying.

% inverts the two players
invertPlayers:-
    nextPlayerHasMoves, %only invert if the next player has valid moves
    player(CurrentPlayer),
    nextPlayer(NextPlayer),
    savePlayer(NextPlayer),
    saveNextPlayer(CurrentPlayer).
invertPlayers.%if next player has no valid move, keep the players

%display board and wait for instruction
nextTurn:-
    evaluateBoard,
    displayBoard,
    repeat,
        waitForInstruction,
    !, endTurn.

%checks the board state, changes the players and starts the nextTurn
endTurn:-
    evaluateBoard, !,
    invertPlayers, !,
    isBotPlaying, !,
    saveHasClaimed(false), % clear the hasClaimed flag.
    nextTurn.

%empties the database and stops the program
exit:-clearInit, abort.
%empties the database
clearInit:-
    abolish(game/1),
    abolish(board/2),
    abolish(player/2),
    abolish(nextPlayer/2),
    abolish(toClaim/2),
    abolish(getColors/3),
    abolish(getStacks/3),
    abolish(hasClaimed/2),

    assert(game(0)), % the only assert needed, others are in utils.pl
    saveHasClaimed(false).

%where everything begins
init:-
    clearInit,
    displayMenu,
    getGameType(GameType),
    startGame(GameType),
    getRandomBoard(Board),

    player(CurrentPlayer),
    nextPlayer(NextPlayer),

    claimableColors(C),
    saveToClaim(C), % load the colors that can be claimed
    saveBoard(Board), % save the board initial state
    saveGetColors(CurrentPlayer, []),
    saveGetColors(NextPlayer, []),
    saveGetStacks(CurrentPlayer, []),
    saveGetStacks(NextPlayer, []),
    !,
    nextTurn,
    clearInit.
