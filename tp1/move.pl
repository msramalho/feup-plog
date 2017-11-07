% file with the predicates for the implementation of the move command
% check if cell is valid (x, y) and print error if not
checkValidCell(X, Y):-isValid(X, Y), !.
checkValidCell(X, Y):-format('Cell(~p, ~p) is not a valid cell in the board\n', [X, Y]), fail.

%fails if stack is empty
checkStackNotEmpty(X, Y):-
    getBoardStackHeight(X, Y, Len),
    Len > 0.

%get all the colors a player can move
getMoveableColorsByPlayer(MoveableColors):-
    player(CurrentPlayer),
    getColors(CurrentPlayer, ClaimedColors),
    toClaim(ToClaim),
    append([ClaimedColors, ToClaim], MoveableColors).%return all the colors the Player can move

%useful to avoid calling getMoveableColorsByPlayer
isColorInStackPlayable(X, Y, AllowedColors):-
    getBoardTopColor(X, Y, TopColor),
    nth0(_, AllowedColors, TopColor). %if the TopColor is one of the colors the player can move


%check if a position belongs to a player, does not load the moveable colors
belongsToPlayer(X, Y):-
    getMoveableColorsByPlayer(MoveableColors), %merge the two lists to get the colors the player can move
    isColorInStackPlayable(X, Y, MoveableColors).

%check if there is a stack in a cell and, if so, if that stack belongs or can be played by the current player (assumes isValid(X, Y))
checkBelongsToPlayer(X, Y):-belongsToPlayer(X, Y).
checkBelongsToPlayer(_,_):-write('You cannot play this stack because of its top color\n'), !, fail.


canPileStacks(Xf, Yf, Xt, Yt):-
    getBoardStackHeight(Xf, Yf, H1),
    getBoardStackHeight(Xt, Yt, H2),
    Sum is H1 + H2,
    Sum =< 5. %only allow them to be piled if the height does not exceed 5
%check if the first stack can go on top of the second
checkStacksPile(Xf, Yf, Xt, Yt):-canPileStacks(Xf, Yf, Xt, Yt), !.
checkStacksPile(_,_,_,_):-write('You cannot pile those two stacks, max height is 5\n'), fail.

trySelect(Find, Initial, Final):-
    select(Find, Initial, Final). %remove wild pieces, because this can be more than one
trySelect(_, S, S).

%same as checkNeutralStackJumpTo but does not print
isNeutralStackJumpTo(Xf, Yf, Xt, Yt):-
    isStackNeutral(Xf, Yf),
    !, %if it is neutral it must respect this rule
    isHeighSmaller(Xf, Yf, Xt, Yt).
isNeutralStackJumpTo(_, _, _, _).%non neutral pieces do not need to obey this rule

%check if the player is trying to move a neutral stack to a higher one RULE E-6, RULE E-7
checkNeutralStackJumpTo(Xf, Yf, Xt, Yt):-
    isStackNeutral(Xf, Yf),
    !, %if it is neutral it must respect this rule
    checkHeightIsSmaller(Xf, Yf, Xt, Yt).
checkNeutralStackJumpTo(_, _, _, _).%non neutral pieces do not need to obey this rule

isHeighSmaller(Xf, Yf, Xt, Yt):-
    getBoardStackHeight(Xf, Yf, H1),
    getBoardStackHeight(Xt, Yt, H2),
    H1 >= H2.
checkHeightIsSmaller(Xf, Yf, Xt, Yt):-isHeighSmaller(Xf, Yf, Xt, Yt).
checkHeightIsSmaller(_, _, _, _):-write('A Neutral Stack (or piece) cannot jump on top of a larger one'), nl, !, fail.


hasNoDuplicateColors(Xf, Yf, Xt, Yt):-
    getBoardStack(Xf, Yf, Stack1),
    getBoardStack(Xt, Yt, Stack2),
    append([Stack1, Stack2], AppendedStacks),
    trySelect(wild, AppendedStacks, AppendedNoWild), %remove wild pieces, because this can be more than one
    remove_dups(AppendedNoWild, Pruned), %make sure there is only one of each color
    length(AppendedNoWild, LenOriginal),
    length(Pruned, LenOriginal). %if not equal than there were duplicates
%check if the stacks have duplicate colors -> cannot be piled if so, fails if no wild
checkDuplicateColors(Xf, Yf, Xt, Yt):-hasNoDuplicateColors(Xf, Yf, Xt, Yt).
checkDuplicateColors(_,_,_,_):-write('You cannot pile because they would have duplicate colors\n'), !, fail.

%after the user inputs, check if it is to abort or to process
processMove(q, _, _, _). %quit if Xf is q
processMove(Xf, Yf, Xt, Yt):-%process move otherwise
    checkValidCell(Xf, Yf),
    checkValidCell(Xt, Yt),
    checkStackNotEmpty(Xt, Yt), !, % RULE E-3
    checkBelongsToPlayer(Xf, Yf), !, % RULE E-1, RULE E-2
    checkStacksPile(Xf, Yf, Xt, Yt), !, % RULE E-5, RULE E-8
    checkNeutralStackJumpTo(Xf, Yf, Xt, Yt), !, % RULE E-6, RULE E-7
    checkDuplicateColors(Xf, Yf, Xt, Yt), !,  % RULE E-5
    checkValidMove(Xf, Yf, Xt, Yt),
    executeMove(Xf, Yf, Xt, Yt).


%prompt for valid X and Y coordinates for origin and destination
move:-
    write('Choose cells FROM and TO using the following format "Xfrom-Yfrom:Xto-Yto." make Xfrom=q to quit\n'),
    read(Xf-Yf:Xt-Yt), !,
    processMove(Xf, Yf, Xt, Yt),
    read_line([]).

%checks if F and T are equal cells
isSameCell(X, Y, X, Y).

%attempts to move recursively in one of the 6 directions, searching in depth only
moveRecursive(Xf, Yf, Xt, Yt):-moveL_Recursive(Xf, Yf, Xt, Yt). % test move left
moveRecursive(Xf, Yf, Xt, Yt):-moveR_Recursive(Xf, Yf, Xt, Yt). % test move right
moveRecursive(Xf, Yf, Xt, Yt):-moveUL_Recursive(Xf, Yf, Xt, Yt). % test move up and left
moveRecursive(Xf, Yf, Xt, Yt):-moveUR_Recursive(Xf, Yf, Xt, Yt). % test move up and right
moveRecursive(Xf, Yf, Xt, Yt):-moveDL_Recursive(Xf, Yf, Xt, Yt). % test move down and left
moveRecursive(Xf, Yf, Xt, Yt):-moveDR_Recursive(Xf, Yf, Xt, Yt). % test move down and right
moveRecursive(_, _, _, _):-
    %write('direct move is not valid'), nl,
    !, fail.

%attempts to move recursively in one of the 6 directions, searching in depth only
moveRecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-moveL_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor). % test move left
moveRecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-moveR_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor). % test move right
moveRecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-moveUL_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor). % test move up and left
moveRecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-moveUR_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor). % test move up and right
moveRecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-moveDL_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor). % test move down and left
moveRecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-moveDR_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor). % test move down and right
moveRecursiveLyngk(_, _, _, _, _, _):-
    %write('unable to execute LYNGK move'), nl,
    !, fail.

%try to go from the beginning until the end, using the lyngk rule if needed
checkValidMove(Xf, Yf, Xt, Yt):-
    isStackOfPlayer(Xf, Yf), !,  %if it belongs to the current player -> can use LYNGK rule
    %write('The Stack Belongs to the player'), nl,
    moveLyngk(Xf, Yf, Xt, Yt).
%try to go from the beginning until the end, only in the same lines
checkValidMove(Xf, Yf, Xt, Yt):-
    %write('The Stack Does NOT Belong to the player'), !, nl,
    moveRecursive(Xf, Yf, Xt, Yt). %test all the 6 move directions depth only search

 %try to move recursively depth only and then try LYNGK move if the first fails
moveLyngk(Xf, Yf, Xt, Yt):-
    moveRecursive(Xf, Yf, Xt, Yt). %test all the 6 move directions depth only search
moveLyngk(Xf, Yf, Xt, Yt):-
    getBoardTopColor(Xf, Yf, LyngkColor), %load the lyngk color to use
    moveRecursiveLyngk(Xf, Yf, Xt, Yt, [Xf-Yf], LyngkColor). %test all the 6 move directions in any order


%is True if Xt, Yt is empty
assertMoveTo(Xt, Yt):- % if empty
    getBoardStack(Xt, Yt, Stack),
    length(Stack, 0).

%is True if Xt, Yt are of the same color (LYNGK) rule
assertMoveToLynkg(Xt, Yt, LyngkColor):- % if same color - LYNGK rule
    getBoardTopColor(Xt, Yt, LyngkColor). %same color can go through, RULE E-4


%----------------------------------------MOVE RECURSIVE START
%-----------------move LEFT Recursive
moveL_Recursive(X, Yf, X, Yt):-%reached the end
    NewYf is Yf - 2,
    isSameCell(X, NewYf, X, Yt).%reached the destination
moveL_Recursive(X, Yf, X, Yt):-%did not reach the end
    NewYf is Yf - 2,
    isValid(X, NewYf), % must be inside the board range, else does not even try
    assertMoveTo(X, NewYf),
    moveL_Recursive(X, NewYf, X, Yt).%go until the next

%-----------------move RIGHT Recursive
moveR_Recursive(X, Yf, X, Yt):-%reached the end
    NewYf is Yf + 2,
    isSameCell(X, NewYf, X, Yt).%reached the destination
moveR_Recursive(X, Yf, X, Yt):-%did not reach the end
    NewYf is Yf + 2,
    isValid(X, NewYf), % must be inside the board range, else does not even try
    assertMoveTo(X, NewYf),
    moveR_Recursive(X, NewYf, X, Yt).%go until the next

%-----------------move UP and LEFT Recursive
moveUL_Recursive(Xf, Yf, Xt, Yt):-%reached the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveUL_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not even try
    assertMoveTo(NewXf, NewYf),
    moveUL_Recursive(NewXf, NewYf, Xt, Yt).%go until the next

%-----------------move UP and RIGHT Recursive
moveUR_Recursive(Xf, Yf, Xt, Yt):-%reached the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveUR_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not even try
    assertMoveTo(NewXf, NewYf),
    moveUR_Recursive(NewXf, NewYf, Xt, Yt).%go until the next

%-----------------move DOWN and LEFT Recursive
moveDL_Recursive(Xf, Yf, Xt, Yt):-%reached the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveDL_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not even try
    assertMoveTo(NewXf, NewYf),
    moveDL_Recursive(NewXf, NewYf, Xt, Yt).%go until the next

%-----------------move DOWN and RIGHT Recursive
moveDR_Recursive(Xf, Yf, Xt, Yt):-%reached the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveDR_Recursive(Xf, Yf, Xt, Yt):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    isValid(NewXf, NewYf), % must be inside the board range, else does not even try
    assertMoveTo(NewXf, NewYf),
    moveDR_Recursive(NewXf, NewYf, Xt, Yt).%go until the next
%----------------------------------------MOVE RECURSIVE END





%----------------------------------------MOVE RECURSIVE LYNGK START
/* isNextLyngkAccessibel()
moveRecursiveBetweenLyngks(Xf, Yf, Xt, Yt, Path, LyngkColor):-
    notInPath(Xf-Yf, Path), % cannot already be in the path (avoid loops)
    assertMoveTo(Xf, Yf), % if this is empty
    findall(NewXt-NewYt, (moveRecursive(Xf, Yf, NewXt, NewYt), getBoardTopColor(NewXt, NewYt, LyngkColor)), Res),
    moveRecursive(Xf, Yf, Xt, Yt), % if the */


moveRecursiveLyngkHelper(Xf, Yf, Xt, Yt, Path, LyngkColor):-%if the new one is empty
    isValid(Xf, Yf), % must be inside the board range, else does not even try
    notInPath(Xf-Yf, Path), % cannot already be in the path (avoid loops)
    assertMoveTo(Xf, Yf), !, %is empty, now it must find the next one in a straight line
    write('path before is: '), write(Path), nl,
    moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY), %tries to find a straight line to lyngk color, updates the path
    write('path after: '), write(Path), nl, nl,
    moveRecursiveLyngk(ResX, ResY, Xt, Yt, NewPath, LyngkColor).%go until the next

moveRecursiveLyngkHelper(Xf, Yf, Xt, Yt, Path, LyngkColor):-%if the new one is NOT empty
    isValid(Xf, Yf), % must be inside the board range, else does not even try
    notInPath(Xf-Yf, Path), % cannot already be in the path (avoid loops)
    assertMoveToLynkg(Xf, Yf, LyngkColor),
    addToPath(Xf-Yf, Path, NewPath), % add to the path so as to avoid circular paths
    moveRecursiveLyngk(Xf, Yf, Xt, Yt, NewPath, LyngkColor).%go until the next

%add a pair of coordinates to the path
addToPath(X-Y, Path, NewPath):-
    append([[X-Y], Path], NewPath).
%fail if the coordinates are in the pair
notInPath(X-Y, Path):-
    nth0(_, Path,  X-Y), !, fail.
notInPath(_, _).
%-----------------move LEFT Recursive
moveL_RecursiveLyngk(X, Yf, X, Yt, _, _):-%reached the end LYNGK
    NewYf is Yf - 2,
    isSameCell(X, NewYf, X, Yt).%reached the destination
moveL_RecursiveLyngk(X, Yf, X, Yt, Path, LyngkColor):-%did not reach the end
    NewYf is Yf - 2,
    moveRecursiveLyngkHelper(X, NewYf, X, Yt, Path, LyngkColor).

%-----------------move RIGHT Recursive LYNGK
moveR_RecursiveLyngk(X, Yf, X, Yt, _, _):-%reached the end
    NewYf is Yf + 2,
    isSameCell(X, NewYf, X, Yt).%reached the destination
moveR_RecursiveLyngk(X, Yf, X, Yt, Path, LyngkColor):-%did not reach the end
    NewYf is Yf + 2,
    moveRecursiveLyngkHelper(X, NewYf, X, Yt, Path, LyngkColor).

%-----------------move UP and LEFT Recursive LYNGK
moveUL_RecursiveLyngk(Xf, Yf, Xt, Yt, _, _):-%reached the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveUL_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    moveRecursiveLyngkHelper(NewXf, NewYf, Xt, Yt, Path, LyngkColor).

%-----------------move UP and RIGHT Recursive LYNGK
moveUR_RecursiveLyngk(Xf, Yf, Xt, Yt, _, _):-%reached the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveUR_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    moveRecursiveLyngkHelper(NewXf, NewYf, Xt, Yt, Path, LyngkColor).

%-----------------move DOWN and LEFT Recursive LYNGK
moveDL_RecursiveLyngk(Xf, Yf, Xt, Yt, _, _):-%reached the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveDL_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    moveRecursiveLyngkHelper(NewXf, NewYf, Xt, Yt, Path, LyngkColor).

%-----------------move DOWN and RIGHT Recursive LYNGK
moveDR_RecursiveLyngk(Xf, Yf, Xt, Yt, _, _):-%reached the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    isSameCell(NewXf, NewYf, Xt, Yt).%reached the destination
moveDR_RecursiveLyngk(Xf, Yf, Xt, Yt, Path, LyngkColor):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    moveRecursiveLyngkHelper(NewXf, NewYf, Xt, Yt, Path, LyngkColor).
%----------------------------------------MOVE RECURSIVE LYNGK END



%----------------------------------------MOVE RECURSIVE LYNGK In STRAGIH LINE START
moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY):-moveLStraight_Recursive(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY).
moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY):-moveRStraight_Recursive(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY).
moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY):-moveULStraight_Recursive(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY).
moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY):-moveURStraight_Recursive(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY).
moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY):-moveDLStraight_Recursive(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY).
moveStraightLineToColor(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY):-moveDRStraight_Recursive(Xf, Yf, Path, LyngkColor, NewPath, ResX, ResY).

moveRecursiveStraightLyngkHelper(Xf, Yf, Path, NewPath):-
    isValid(Xf, Yf), % must be inside the board range, else does not even try
    notInPath(Xf-Yf, Path), % cannot already be in the path (avoid loops)
    assertMoveTo(Xf, Yf),
    addToPath(Xf-Yf, Path, NewPath).% add to the path so as to avoid circular paths
%-----------------move LEFT Recursive
moveLStraight_Recursive(ResX, Yf, _, LyngkColor, _, ResX, ResY):-%reached the end
    ResY is Yf - 2,
    getBoardTopColor(ResX, ResY, LyngkColor).%reached the destination
moveLStraight_Recursive(Xf, Yf, Path, LyngkColor, FinalPath, ResX, ResY):-%did not reach the end
    NewYf is Yf - 2,
    moveRecursiveStraightLyngkHelper(Xf, NewYf, Path, NewPath),
    moveLStraight_Recursive(Xf, NewYf, NewPath, LyngkColor, FinalPath, ResX, ResY).
%-----------------move RIGHT Recursive
moveRStraight_Recursive(ResX, Yf, _, LyngkColor, _, ResX, ResY):-%reached the end
    ResY is Yf + 2,
    getBoardTopColor(ResX, ResY, LyngkColor).%reached the destination
moveRStraight_Recursive(Xf, Yf, Path, LyngkColor, FinalPath, ResX, ResY):-%did not reach the end
    NewYf is Yf + 2,
    moveRecursiveStraightLyngkHelper(Xf, NewYf, Path, NewPath),
    moveRStraight_Recursive(Xf, NewYf, NewPath, LyngkColor, FinalPath, ResX, ResY).
%-----------------move UP and LEFT Recursive
moveULStraight_Recursive(Xf, Yf, _, LyngkColor, _, ResX, ResY):-%reached the end
    ResX is Xf - 1,
    ResY is Yf - 1,
    getBoardTopColor(ResX, ResY, LyngkColor).%reached the destination
moveULStraight_Recursive(Xf, Yf, Path, LyngkColor, FinalPath, ResX, ResY):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf - 1,
    moveRecursiveStraightLyngkHelper(NewXf, NewYf, Path, NewPath),
    moveULStraight_Recursive(NewXf, NewYf, NewPath, LyngkColor, FinalPath, ResX, ResY).
%-----------------move UP and RIGHT Recursive
moveURStraight_Recursive(Xf, Yf, _, LyngkColor, _, ResX, ResY):-%reached the end
    ResX is Xf - 1,
    ResY is Yf + 1,
    getBoardTopColor(ResX, ResY, LyngkColor).%reached the destination
moveURStraight_Recursive(Xf, Yf, Path, LyngkColor, FinalPath, ResX, ResY):-%did not reach the end
    NewXf is Xf - 1,
    NewYf is Yf + 1,
    moveRecursiveStraightLyngkHelper(NewXf, NewYf, Path, FinalPath, NewPath),
    moveURStraight_Recursive(NewXf, NewYf, NewPath, LyngkColor, FinalPath, ResX, ResY).
%-----------------move DOWN and LEFT Recursive
moveDLStraight_Recursive(Xf, Yf, _, LyngkColor, _, ResX, ResY):-%reached the end
    ResX is Xf + 1,
    ResY is Yf - 1,
    getBoardTopColor(ResX, ResY, LyngkColor).%reached the destination
moveDLStraight_Recursive(Xf, Yf, Path, LyngkColor, FinalPath, ResX, ResY):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf - 1,
    moveRecursiveStraightLyngkHelper(NewXf, NewYf, Path, NewPath),
    moveDLStraight_Recursive(NewXf, NewYf, NewPath, LyngkColor, FinalPath, ResX, ResY).
%-----------------move DOWN and RIGHT Recursive
moveDRStraight_Recursive(Xf, Yf, _, LyngkColor, _, ResX, ResY):-%reached the end
    ResX is Xf + 1,
    ResY is Yf + 1,
    getBoardTopColor(ResX, ResY, LyngkColor).%reached the destination
moveDRStraight_Recursive(Xf, Yf, Path, LyngkColor, FinalPath, ResX, ResY):-%did not reach the end
    NewXf is Xf + 1,
    NewYf is Yf + 1,
    moveRecursiveStraightLyngkHelper(NewXf, NewYf, Path, NewPath),
    moveDRStraight_Recursive(NewXf, NewYf, NewPath, LyngkColor, FinalPath, ResX, ResY).





%----------------------------------------MOVE RECURSIVE LYNGK In STRAGIH LINE END










executeMove(Xf, Yf, Xt, Yt):-
    board(B),
    getBoardStack(Xf, Yf, StackTop),
    getBoardStack(Xt, Yt, StackBottom),
    append([StackTop, StackBottom], Stack), % RULE E-2
    replaceBoardStack(B, Xt, Yt, Stack, B2),
    replaceBoardStack(B2, Xf, Yf, [], NewBoard),
    saveBoard(NewBoard).
