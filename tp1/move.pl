
% check if cell is valid (x, y) and print error if not
checkValidCell(X, Y):-isValid(X, Y), !.
checkValidCell(X, Y):-format('Cell(~p, ~p) is not a valid cell in the board\n', [X, Y]), fail.

%fails if stack is empty
checkStackNotEmpty(X, Y):-
    getBoardStackHeight(X, Y, Len),
    Len >= 0.

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
    write('You cannot play this stack because of its top color\n'), !, fail.


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
    select(Find, Initial, Final). %remove wild pieces, because this can be more than one
trySelect(_, S, S).

/* %check if a neutral single piece is being moved to a valid place RULE E-6
checkSingleNeutralRule(Xf, Yf, Xt, Yt):-
    getBoardStackHeight(Xf, Yf, 1), %single piece
    isStackNeutral(Xf, Yf), % test if this stack is neutral
    !, %cut, meaning that if the board height is one then it must respect the following:
    checkSingleNeutralRuleSecondStack(Xt, Yt). %only True if the destination is a single piece too
checkSingleNeutralRule(_, _, _, _). % True if the stack is higher than one

checkSingleNeutralRuleSecondStack(Xt, Yt):-
    getBoardStackHeight(Xt, Yt, 1). %only True if the destination is a single piece too
checkSingleNeutralRuleSecondStack(_, _):-
    write('A Single Neutral Piece can only move onto single pieces'), nl, !, fail. */

%check if the player is trying to move a neutral stack to a higher one RULE E-6, RULE E-7
checkNeutralStackJumpTo(Xf, Yf, Xt, Yt):-
    isStackNeutral(Xf, Yf),
    !, %if it is neutral it must respect this rule
    checkHeightIsSmaller(Xf, Yf, Xt, Yt).
checkNeutralStackJumpTo(_, _, _, _).%non neutral pieces do not need to obey this rule

checkHeightIsSmaller(Xf, Yf, Xt, Yt):-
    getBoardStackHeight(Xf, Yf, H1),
    getBoardStackHeight(Xt, Yt, H2),
    H1 >= H2.
checkHeightIsSmaller(_, _, _, _):-
    write('A Neutral Stack (or piece) cannot jump on top of a larger one'), nl, !, fail.


%check if the stacks have duplicate colors -> cannot be piled if so, fails if no wild
checkDuplicateColors(Xf, Yf, Xt, Yt):-
    getBoardStack(Xf, Yf, Stack1),
    getBoardStack(Xt, Yt, Stack2),
    append([Stack1, Stack2], AppendedStacks),
    trySelect(wild, AppendedStacks, AppendedNoWild),%remove wild pieces, because this can be more than one
    remove_dups(AppendedNoWild, Pruned),%make sure there is only one of each color
    length(AppendedNoWild, LenOriginal),
    length(Pruned, LenPruned),
    LenOriginal = LenPruned, !, %if not equal than there were duplicates
    write('Not Duplicate!'), nl.

checkDuplicateColors(_,_,_,_):-
    write('You cannot pile because they would have duplicate colors\n'), !, fail.


%prompt for valid X and Y coordinates for origin and destination
move(Xf, Yf, Xt, Yt):-
    write('Choose cells FROM and TO using the following format "Xfrom-Yfrom:Xto-Yto."\n'),
    read(Xf-Yf:Xt-Yt),
    checkValidCell(Xf, Yf),
    checkValidCell(Xt, Yt),
    checkStackNotEmpty(Xt, Yt), !, % RULE E-3
    checkBelongsToPlayer(Xf, Yf), !, % RULE E-1, RULE E-2
    checkStacksPile(Xf, Yf, Xt, Yt), !, % RULE E-5, RULE E-8
    %checkSingleNeutralRule(Xf, Yf, Xt, Yt), !, % RULE E-6
    checkNeutralStackJumpTo(Xf, Yf, Xt, Yt), !, % RULE E-6, RULE E-7
    checkDuplicateColors(Xf, Yf, Xt, Yt), % RULE E-5
    checkValidMove(Xf, Yf, Xt, Yt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%checkValidMove/4: check if can go from Xf, Yf, Xt, Yt

%checks if F and T are equal cells
isSameCell(X, Y, X, Y).

%attempts to move recursively in one of the 6 directions
moveRecursive(Xf, Yf, Xt, Yt):-moveL_Recursive(Xf, Yf, Xt, Yt). % test move left
moveRecursive(Xf, Yf, Xt, Yt):-moveR_Recursive(Xf, Yf, Xt, Yt). % test move right
moveRecursive(Xf, Yf, Xt, Yt):-moveUL_Recursive(Xf, Yf, Xt, Yt). % test move up and left
moveRecursive(Xf, Yf, Xt, Yt):-moveUR_Recursive(Xf, Yf, Xt, Yt). % test move up and right
moveRecursive(Xf, Yf, Xt, Yt):-moveDL_Recursive(Xf, Yf, Xt, Yt). % test move down and left
moveRecursive(Xf, Yf, Xt, Yt):-moveDR_Recursive(Xf, Yf, Xt, Yt). % test move down and right
moveRecursive(_, _, _, _):- write('not able to move recursively'), nl, !, fail.

%the default case, from and to are the same
checkValidMove(Xf, Yf, Xt, Yt):-isSameCell(Xf, Yf, Xt, Yt).
%try to go from the beginning until the end, only in the same lines
checkValidMove(Xf, Yf, Xt, Yt):-
    moveRecursive(Xf, Yf, Xt, Yt), %test all the 6 move directions depth only search
    executeMove(Xf, Yf, Xt, Yt).
%try to go from the beginning until the end, using the lyngk rule if needed
checkValidMove(Xf, Yf, Xt, Yt):-
    isStackOfPlayer(Xf, Yf), %if it belongs to the current player -> can use LYNGK rule
    moveLyngk(Xf, Yf, Xt, Yt),
    executeMove(Xf, Yf, Xt, Yt).
/* TODO: uncomment and make this work
moveLyngk(Xf, Yf, Xt, Yt):-
    moveRecursive(Xf, Yf, Xt, Yt). %test all the 6 move directions depth only search
moveLyngk(Xf, Yf, Xt, Yt):-
    moveRecursiveLyngk(Xf, Yf, Xt, Yt). %test all the 6 move directions in any order
 */
%is True if Xt, Yt is empty
assertMoveTo(Xf, Yf, Xt, Yt):- % if empty
    getBoardStack(Xt, Yt, Stack),
    length(Stack, 0).

%is True if Xt, Yt is empty or if they are of the same color (LYNGK) rule
assertMoveToLynkg(Xf, Yf, Xt, Yt):-
    assertMoveTo(Xf, Yf, Xt, Yt).
assertMoveToLynkg(Xf, Yf, Xt, Yt):- % if same color - LYNGK rule
    getBoardTopColor(Xf, Yf, C1),
    getBoardTopColor(Xt, Yt, C1). %same color can go through, RULE E-4

%----------------------------------------MOVE RECURSIVE Start
%-----------------move LEFT Recursive
moveL_Recursive(X, Yf, X, Yt):-%reched the end
    NewYf is Yf - 2,
    isSameCell(X, NewYf, X, Yt).%reached the destination
moveL_Recursive(X, Yf, X, Yt):-%did not reach the end
    NewYf is Yf - 2,
    isValid(X, NewYf), % must be inside the board range, else does not env try
    assertMoveTo(X, Yf, X, NewYf),
    moveL_Recursive(X, NewYf, X, Yt).%go until the next

%-----------------move RIGHT Recursive
moveR_Recursive(X, Yf, X, Yt):-%reched the end
    NewYf is Yf + 2,
    isSameCell(X, NewYf, X, Yt).%reached the destination
moveR_Recursive(X, Yf, X, Yt):-%did not reach the end
    NewYf is Yf + 2,
    isValid(X, NewYf), % must be inside the board range, else does not env try
    assertMoveTo(X, Yf, X, NewYf),
    moveR_Recursive(X, NewYf, X, Yf).%go until the next

%-----------------move UP and LEFT Recursive
moveUL_Recursive(Xf, Yf, Xt, Yt):-%reched the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveUL_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not env try
    assertMoveTo(Xf, Yf, NewXf, NewYf),
    moveUL_Recursive(NewXf, NewYf, Xt, Yt).%go until the next

%-----------------move UP and RIGHT Recursive
moveUR_Recursive(Xf, Yf, Xt, Yt):-%reched the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveUR_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not env try
    assertMoveTo(Xf, Yf, NewXf, NewYf),
    moveUR_Recursive(NewXf, NewYf, Xt, Yt).%go until the next

%-----------------move DOWN and LEFT Recursive
moveDL_Recursive(Xf, Yf, Xt, Yt):-%reched the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveDL_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not env try
    assertMoveTo(Xf, Yf, NewXf, NewYf),
    moveDL_Recursive(NewXf, NewYf, Xt, Yt).%go until the next

%-----------------move DOWN and RIGHT Recursive
moveDR_Recursive(Xf, Yf, Xt, Yt):-%reched the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveDR_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not env try
    assertMoveTo(Xf, Yf, NewXf, NewYf),
    moveDR_Recursive(NewXf, NewYf, Xt, Yt).%go until the next
%----------------------------------------MOVE RECURSIVE END



executeMove(Xf, Yf, Xt, Yt):-
    board(B),
    getBoardStack(Xf, Yf, StackTop),
    getBoardStack(Xt, Yt, StackBottom),
    append([StackTop, StackBottom], Stack), % RULE E-2
    replaceBoardStack(B, Xt, Yt, Stack, B2),
    replaceBoardStack(B2, Xf, Yf, [], NewBoard),
    retract(board(_)),
    assert(board(NewBoard)).

/* checkIsValidPath(Xf, Yf, Xt, Yt),
    getBoardTopColor(Xf, Yf, FromC) */

/*     moveUL(Xf, Yf, Xt, Yt);
    moveUR(Xf, Yf, Xt, Yt); */