
% check if cell is valid (x, y) and print error if not
checkValidCell(X, Y):-isValid(X, Y), !.
checkValidCell(X, Y):-format('Cell(~p, ~p) is not a valid cell in the board\n', [X, Y]), fail.


getMoveableColorsByPlayer(Player, MoveableColors):-
    getColors(Player, ClaimedColors),
    toClaim(ToClaim),
    append([ClaimedColors, ToClaim], MoveableColors).%return all the colors the Player can move

%check if there is a stack in a cell and, if so, if that stack belongs or can be played by the current player (assumes isValid(X, Y))
checkBelongsToPlayer(X, Y):-
    getBoardTopColor(X, Y, TopColor),
    TopColor \= wild,!,
    player(CurrentPlayer),
    getMoveableColorsByPlayer(CurrentPlayer, MoveableColors),%merge the two lists to get the colors the player can move
    nth0(_, MoveableColors, TopColor). %if the TopColor is one of the colors the player can move
checkBelongsToPlayer(_,_):-
    write('You cannot play this stack because of its top color\n'), fail.


%check if the first stack can go on top of the second
checkStacksPile(Xf, Yf, Xt, Yt):-
    getBoardStackHeight(Xf, Yf, H1),
    getBoardStackHeight(Xt, Yt, H2),
    Sum is H1 + H2,
    !,
    Sum =< 5. %only allow them to be piled if the height does not exceed 5

checkStacksPile(_,_,_,_):-
    write('You cannot pile those two stacks, max height is 5\n'), fail.

trySelect(Find, Initial, Final):-
    select(Find, Initial, Final),%remove wild pieces, because this can be more than one
trySelect(_, S, S).

%check if the stacks have duplicate colors -> cannot be piled if so, fails if no wild
checkDuplicateColors(Xf, Yf, Xt, Yt):-
    getBoardStack(Xf, Yf, Stack1),
    getBoardStack(Xt, Yt, Stack2),
    append([Stack1, Stack2], AppendedStacks),
    trySelect(wild, AppendedStacks, AppendedNoWild),%remove wild pieces, because this can be more than one
    remove_dups(AppendedNoWild, Pruned),%make sure there is only one of each color
    length(AppendedNoWild, LenOriginal),
    length(Pruned, LenPruned),
    !,
    LenOriginal = LenPruned.%if not equal than there were duplicates

checkDuplicateColors(_,_,_,_):-
    write('You cannot pile because they would have duplicate colors\n'), fail.


%prompt for valid X and Y coordinates for origin and destination
move(Xf, Yf, Xt, Yt):-
    write('Choose cells FROM and TO using the following format "Xfrom-Yfrom:Xto-Yto."\n'),
    read(Xf-Yf:Xt-Yt),
    checkValidCell(Xf, Yf),
    checkValidCell(Xt, Yt),
    checkBelongsToPlayer(Xf, Yf),
    checkStacksPile(Xf, Yf, Xt, Yt),
    checkDuplicateColors(Xf, Yf, Xt, Yt),
    checkValidMove(Xf, Yf, Xt, Yt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%checkValidMove/4: check if can go from Xf, Yf, Xt, Yt

%checks if F and T are equal cells
isSameCell(Xf, Yf, Xt, Yt):-
    Xf =:= Xt,
    Xt =:= Yt.
%the default case, from and to are the same
checkValidMove(Xf, Yf, Xt, Yt):-isSameCell(Xf, Yf, Xt, Yt).

%attempts to move recursively in one of the 6 directions
moveRecursive(Xf, Yf, Xt, Yt):-moveL_Recursive(Xf, Yf, Xt, Yt). % test move left
/* moveRecursive(Xf, Yf, Xt, Yt):-moveR_Recursive(Xf, Yf, Xt, Yt). % test move right
moveRecursive(Xf, Yf, Xt, Yt):-moveUL_Recursive(Xf, Yf, Xt, Yt). % test move up and left
moveRecursive(Xf, Yf, Xt, Yt):-moveUR_Recursive(Xf, Yf, Xt, Yt). % test move up and right
moveRecursive(Xf, Yf, Xt, Yt):-moveDL_Recursive(Xf, Yf, Xt, Yt).% test move down and left
moveRecursive(Xf, Yf, Xt, Yt):-moveDR_Recursive(Xf, Yf, Xt, Yt). */

%try to go from the beginning until the end
checkValidMove(Xf, Yf, Xt, Yt):-
    moveRecursive(Xf, Yf, Xt, Yt), %test all the 6 move directions recursively
    executeMove(Xf, Yf, Xt, Yt).

%is True if Xt, Yt is empty or if they are of the same color (LYNGK) rule
assertMoveTo(Xf, Yf, Xt, Yt):- % if empty
    getBoardStack(Xt, Yt, Stack),
    Stack = [].
assertMoveTo(Xf, Yf, Xt, Yt):- % if same color - LYNGK rule
    getBoardTopColor(Xf, Yf, C1),
    getBoardTopColor(Xt, Yt, C2),
    C1 = C2. %same color can go through

%-----------------moveL_Recursive
moveL_Recursive(Xf, Y, Xt, Y):-%reched the end
    NewXf is Xf - 2,
    isSameCell(NewXf, Y, Xt, Y).
moveL_Recursive(Xf, Y, Xt, Y):-%did not reach the ed
    T_NewXf is Xf - 2,
    isValid(T_NewXf, Y), % must be inside the board range, else does not env try
    assertMoveTo(T_NewXf, Y, Xt, Y),
    moveL_Recursive(T_NewXf, Y, Xt, Y, NewXf, Y).%go until the next



executeMove(Xf, Yf, Xt, Yt):-
    board(B),
    getBoardStack(Xf, Yf, StackTop),
    getBoardStack(Xt, Yt, StackBottom),
    append([StackTop, StackBottom], Stack),
    replaceBoardStack(B, Xt, Yt, Stack, NewBoard),
    retract(board(_)),
    assert(board(NewBoard)).

/* checkIsValidPath(Xf, Yf, Xt, Yt),
    getBoardTopColor(Xf, Yf, FromC) */

/*     moveUL(Xf, Yf, Xt, Yt);
    moveUR(Xf, Yf, Xt, Yt); */