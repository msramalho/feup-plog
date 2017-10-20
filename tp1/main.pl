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
    p1Colors/1,     % a list of the colors claimed by player1
    p2Colors/1,     % a list of the colors claimed by player2
    p1Stacks/1,     % a list of the stacks collected by player1
    p2Stacks/1.     % a list of the stacks collected by player2

:- dynamic
    board/1,
    player/1,
    p1Colors/1,
    p2Colors/1,
    p1Stacks/1,
    p2Stacks/1.

%game type (User x User | User x Bot)
startGame(GameType):-GameType = quit, abort. %abrt
startGame(GameType):-
    GameType = humanVhuman,
    write('Human Vs Human Selected') .
startGame(GameType):-
    GameType = humanVbot,
    write('Human Vs Bot Selected') .


%try to read a valid game type (1(humanVhuman), 2(humanVbot) or 3 (quit))
getGameType(GameType):-
    read_line([GameTypeLine|_]),
    menuTranslate(GameType, GameTypeLine).
getGameType(GameType):-
    write('Wrong game type, try again:\n'),
    getGameType(GameType).

init:-
    displayMenu,
    getGameType(GameType),
    startGame(GameType).