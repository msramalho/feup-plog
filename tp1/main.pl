:- include('utils.pl').

:- include('getRandomBoard.pl').
:- include('displayBoard.pl').

:- include('evaluate.pl').
:- include('interface.pl').

%:- include('tests.pl').

:-setRandomSeed. %set a random seed


% volatile and dynamic predicates declaration
:- volatile
    board/1,        % the current board state
    player/1,       % the current player
    nextPlayer/1,   % the next player
    toClaim/1,      % which colors are yet to claim
    getColors/2,    % a list of the colors claimed by the selected player getColors(player, Colors)
    getStacks/2.    % a list of the stacks collected by the selected player getStacks(player, Stacks)

:- dynamic
    board/1,
    player/1,
    nextPlayer/1,
    toClaim/1,
    getColors/2,
    getStacks/2.

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
startGame(GameType):-GameType = quit, abort. %abort
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
        getMoveCoordinates(Xf, Yf, Xt, Yt),
    write('got coordinates').
    %format('Moving from ~d,~d to ~d,~d\n', [Xf, Yf, Xt, Yt]).

%expecting a claim instruction
parseInstruction(Instruction):-
    Instruction = "claim",
    write('which color would you like to claim?\n'),
    read(Color),
    checkClaimableColor(Color).
    %execute claim Color

parseInstruction(Instruction):-
    Instruction = "quit",
    abort.

parseInstruction(_):-
    write('Instruction not recognized, try again.\n'), fail.

%where everything begins
init:-
    displayMenu,
    getGameType(GameType),
    startGame(GameType),
    getRandomBoard(Board),
    player(CurrentPlayer),
    nextPlayer(NextPlayer),
    assert(toClaim(claimableColors)), % load the colors that can be claimed
    assert(board(Board)),   %save the board state
    assert(getColors(CurrentPlayer, [empty, empty])),
    assert(getColors(NextPlayer, [empty, empty])),
    assert(getStacks(CurrentPlayer, [])),
    assert(getStacks(NextPlayer, [])),
    !,
    displayBoard,
    repeat,
        waitForInstruction.

