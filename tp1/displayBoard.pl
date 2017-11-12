/*
displayBoard.pl: This file implements the predicates required for displaying the board
*/

concatenateList([], Result, Result).
concatenateList([Color|NextColors], Initial, Final):-
	translate(Color, Value),
	atom_concat(Initial, Value, Result),
	concatenateList(NextColors, Result, Final).

%decides whether the next cell is valid or not and writes dashes if so or spaces if not
writeBetweenValid(LineIndex, CellIndex):-
    isValid(LineIndex, CellIndex),
    wd(5).
writeBetweenValid(_, _):-
    write('     ').

%final state of the state machine, enters it after first appearance of a valid cell
stateReceiveValidCell([], _, _).
stateReceiveValidCell([ColorsValid, _, ColorsUnknown|Rest], LineIndex, CellIndex):- % while a valid is passed stay in this state, has 3 elements, sort of a peek into the next two cells to see if dashes are an option
	isValid(LineIndex, CellIndex),
	concatenateList(ColorsValid, '', Concatenated),
    format('~|~t~s~t~5+', Concatenated),
	NewCellIndex is CellIndex + 2,
    writeBetweenValid(LineIndex, NewCellIndex),
	stateReceiveValidCell([ColorsUnknown|Rest], LineIndex, NewCellIndex).
stateReceiveValidCell([PotentialLastColor|Rest], LineIndex, CellIndex):-
	isValid(LineIndex, CellIndex),
	concatenateList(PotentialLastColor, '', Concatenated),
    format('~|~t~s~t~5+', Concatenated),
	NewCellIndex is CellIndex + 1,
	stateReceiveValidCell(Rest, LineIndex, NewCellIndex).
stateReceiveValidCell([_|Rest], LineIndex, CellIndex):-
    write('     '),
	NewCellIndex is CellIndex + 1,
	stateReceiveValidCell(Rest, LineIndex, NewCellIndex).

%initial state of the state machine, stays while there is no valid cell
stateIgnoreInvalidCell(Cells, LineIndex, CellIndex):-%leave state if a valid is found
	isValid(LineIndex, CellIndex),
	stateReceiveValidCell(Cells, LineIndex, CellIndex).

stateIgnoreInvalidCell([_|Rest], LineIndex, CellIndex):-%receive all the invalid
    write('     '),
	NewCellIndex is CellIndex + 1,
	stateIgnoreInvalidCell(Rest, LineIndex, NewCellIndex).

displaySlashesStart([], _, _).
displaySlashesStart(_, LineIndex, CellIndex):-
    isValid(LineIndex, CellIndex),
    List = [0, 3, 6, 7, 9, 10],
    nth0(_, List, LineIndex),
    write('     ').
displaySlashesStart(_, LineIndex, CellIndex):-
    isValid(LineIndex, CellIndex).
displaySlashesStart([_|Rest], LineIndex, CellIndex):-
    NewCellIndex is CellIndex + 1,
    displaySlashesStart(Rest, LineIndex, NewCellIndex),
    write('     ').

%minimum between two numbers
minBetween(X, Y, Result):- X < Y, X = Result.
minBetween(_, Y, Result):- Y = Result.

%alternate the kind of slashes according to even and odd lines
displaySlashes(_, CountValidCurrent, CountValidNext):-
    minBetween(CountValidCurrent, CountValidNext, Min),
    Min =:= CountValidCurrent,
    writeString('/    \\    ', CountValidCurrent).

displaySlashes(_, _, CountValidNext):- %odd lines
    writeString('\\    /    ', CountValidNext).

%handle the display of a line with slashes
displaySlashesLine(LineIndex):-
    findall(X, isValid(LineIndex, X), L1),
    length(L1, CountValidCurrent),
    NewLineIndex is LineIndex + 1,
    findall(X, isValid(NewLineIndex, X), L2),
    length(L2, CountValidNext),
    displaySlashes(LineIndex, CountValidCurrent, CountValidNext).


%displays a line of the board and calls itself recursively
displayLine([], _).
displayLine([Line|Rest], LineIndex):-
	numberToString(LineIndex, LineString),	%convert the lineIndex to a string
	format('~2s  ', LineString),				%print the index with the right padding
	stateIgnoreInvalidCell(Line, LineIndex, 0), nl,
    write('   '),
    displaySlashesStart(Line, LineIndex, 0),
    displaySlashesLine(LineIndex), nl,
	NewLineIndex is LineIndex + 1,
	displayLine(Rest, NewLineIndex).

displayPlayerStats(Player):-
    getColors(Player, Colors),
	format('~s\n    colors: ', Player), write(Colors),
    getStacks(Player, Stacks),
    write('\n    stacks: '), write(Stacks),
    evaluateBoard(Player, Score),
    write('\n    Score: '), write(Score).

%clears the screen and ouputs the board and the game state
displayBoard:-
    clear,
	write('      0    1    2    3    4    5    6    7    8\n\n'),
    board(Board),
	displayLine(Board, 0),
	wd(66),
    nl, !,

    player(CurrentPlayer),
	write('\nNow playing:'),
    displayPlayerStats(CurrentPlayer),

    nextPlayer(NextPlayer),
	write('\nNext up:'),
    displayPlayerStats(NextPlayer),

    toClaim(C),
	write('\nAvailable Colors:'), write(C), nl, nl, nl.