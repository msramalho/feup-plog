:- include('utils.pl').

:- include('getRandomBoard.pl').
:- include('displayBoard.pl').

:- include('evaluate.pl').
:- include('claim.pl').
:- include('move.pl').
:- include('interface.pl').

%:- include('tests.pl').


% volatile and dynamic predicates declaration
:- volatile
    board/1,        % the current board state
    player/1,       % the current player
    nextPlayer/1,   % the next player
    toClaim/1,      % which colors are yet to claim
    getColors/2,    % a list of the colors claimed by the selected player getColors(player, Colors)
    getStacks/2.    % a list of the stacks collected by the selected player getStacks(player, Stacks)
    hasClaimed/0.   % a flag to indicate wheter the current player has claimed a color in this turn

:- dynamic
    board/1,
    player/1,
    nextPlayer/1,
    toClaim/1,
    getColors/2,
    getStacks/2,
    hasClaimed/0.

%make bot start first or human start first, 50% chance
randomizeBotPlay:-
    random_select(Temp, [0, 1], _),
    Temp =:= 0,
    assert(player(player1)),
    assert(nextPlayer(bot)),
    write('Player 1 goes first...\n').
randomizeBotPlay:-
    assert(player(bot)),
    assert(nextPlayer(player1)),
    write('Bot goes first...\n').

%game type (User x User | User x Bot)
startGame(GameType):-GameType = quit, exit. %abort
startGame(GameType):-GameType = instructions, displayInstructions. %abort
startGame(GameType):- %intialize both players. The real players should randomly choose their turn
    GameType = humanVhuman,
    assert(player(player1)),
    assert(nextPlayer(player2)),
    write('Human Vs Human Selected\n').

startGame(GameType):- % initialize the player and the nextPlayer randomly, the bot may be first
    GameType = humanVbot,
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
parseInstruction(Instruction):-
    Instruction = "move",
    repeat,
        move(Xf, Yf, Xt, Yt),
    !,
    write('moved'),
    nextTurn.
    %format('Moving from ~d,~d to ~d,~d\n', [Xf, Yf, Xt, Yt]).

%expecting a claim instruction
parseInstruction(Instruction):-%instruction was claim and has not claimed any piece
    Instruction = "claim",
    not(hasClaimed),
    repeat,
        claimColor,
    !,
    nextTurn.
parseInstruction(Instruction):-%claim but already claimed a piece in this turn
    Instruction = "claim",
    write('You can only claim one color per turn\n'), !,  fail.

%expecting a quit instruction
parseInstruction(Instruction):-
    Instruction = "quit",
    exit.

%unexpected instruction
parseInstruction(_):-
    write('Instruction not recognized, try again.\n'), fail.

%display board and wait for instruction
nextTurn:-
    write('displaying board\n'),
    displayBoard,
    repeat,
        waitForInstruction,
    !.

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
    retract(player(_)),
    retract(nextPlayer(_)),
    assert(player(NextPlayer)),
    assert(nextPlayer(CurrentPlayer)).
invertPlayers.%if next player has no valid move, keep the players

%checks the board state, changes the players and starts the nextTurn
endTurn:-
    evaluateBoard,
    invertPlayers,
    isBotPlaying,
    abolish(hasClaimed/0), % clear the hasClaimed flag
    nextTurn.

%empties the database and stops the program
exit:-clearInit, abort.
%empties the database
clearInit:-
    abolish(board/1),
    abolish(player/1),
    abolish(nextPlayer/1),
    abolish(toClaim/1),
    abolish(getColors/2),
    abolish(getStacks/2),
    abolish(hasClaimed/0).

%where everything begins
init:-
    displayMenu,
    getGameType(GameType),
    startGame(GameType),
    getRandomBoard(Board),

    player(CurrentPlayer),
    nextPlayer(NextPlayer),

    claimableColors(C),
    assert(toClaim(C)),     % load the colors that can be claimed
    assert(board(Board)),   % save the board state
    assert(getColors(CurrentPlayer, [])),
    assert(getColors(NextPlayer, [])),
    assert(getStacks(CurrentPlayer, [])),
    assert(getStacks(NextPlayer, [])),
    !,
    nextTurn,
    clearInit.
