:- include('utils.pl').

:- include('getRandomBoard.pl').
:- include('displayBoard.pl').

:- include('evaluate.pl').
:- include('interface.pl').

:- include('tests.pl').

:-setRandomSeed. %set a random seed


% volatile and dynamic predicates declaration
:- volatile
    board/1,        % the current board state
    player/1,       % the current player
    nextPlayer/1,   % the next player
    p1Colors/1,     % a list of the colors claimed by player1
    p2Colors/1,     % a list of the colors claimed by player2
    p1Stacks/1,     % a list of the stacks collected by player1
    p2Stacks/1.     % a list of the stacks collected by player2

:- dynamic
    board/1,
    player/1,
    nextPlayer/1,
    p1Colors/1,
    p2Colors/1,
    p1Stacks/1,
    p2Stacks/1.

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

waitForInstruction:-
    read_line("move"),
    write('moving from (Xfrom-Yfrom:Xto-Yto.):'),
    read(Xf-Yf:Xt-Yt),
    format('from ~d,~d to ~d,~d', [Xf, Yf, Xt, Yt]).
    %format('Moving from ~d,~d to ~d,~d\n', [Xf, Yf, Xt, Yt]).

waitForInstruction:-
    read_line(Instruction),
    Instruction = '3'.

waitForInstruction:-
    write('instruction not recognized\n'),
    waitForInstruction.

%where everything begins
init:-
    displayMenu,
    getGameType(GameType),
    startGame(GameType),
    getRandomBoard(Board),
    assert(board(Board)),   %save the board state
    write('The Game is on\n'),
    waitForInstruction.