/*
interface.pl: predicates related to the color menus and special character printing predicates
*/

wulc:-put_code(9484). % write upper left corner ┌
wurc:-put_code(9488). % write upper right corner ┐
wllc:-put_code(9492). % write lower left corner ┘
wlrc:-put_code(9496). % write lower right corner └
wlf:-put_code(9500). % write left fork ├
wrf:-put_code(9508). % write right fork ┤
wvd:-put_code(9474). % write vertical dash│

wd:-put_code(9472). % write horizontal dash ─
wd(N):-N=<0, wd.
wd(N):-wd, N1 is N-1, wd(N1).

% write dashed separator line
wsl(Len):-wlf, wd(Len), wrf, nl.


% write line with content center aligned
wel(Len):-wel(Len, ' ').
wel(Len, Content):-
	Start = '~|~t~s~t~', End = '+',
	atom_concat(Start, Len, S1), atom_concat(S1, End, S2),
	wvd, format(S2, Content), wvd, nl.

displayMenu:-
	write('\33\[2J'),
	wulc, wd(39), wurc, nl,
	wel('30'),
	wel('30', 'LYNGK GAME'),
	wel('30'),
	wsl(39),
	wel('30'), wel('30'),wel('30'),
	wel('30', '1: Human Vs Human'),
	wel('30', '2: Human Vs Bot'),
	wel('30', '3: Bot Vs Bot'),
	wel('30', '4: Quit'),
	wel('30', '5: Instructions'),
	wel('30'), wel('30'),wel('30'),
	wllc, wd(39), wlrc, nl.

displayInstructions:-
	write('\33\[2J'),
	wulc, wd(67), wurc, nl,
	wel('51'),
	wel('51', 'Instructions'),
	wel('51'),
	wsl(67),
	wel('51'), wel('51'),wel('51'),
	wel('51', 'WHEN PLAYING, choose one of:'),
	wel('51', 'move, claim'),
	wel('51', 'and then press enter'), wel('51'),
	wel('51', 'for move specify:'),
	wel('51', 'Xfrom-Yfrom:Xto-Yto'), wel('51'),
	wel('51', 'for claim specify:'),
	wel('51', 'colorName'), wel('51'), wel('51'),
	wel('51', 'Enter to go back'),
	wel('51'), wel('51'),wel('51'),
	wllc, wd(67), wlrc, nl,
    waitForEnter,
    init.

displayBotLevels(Bot):-
	write('\33\[2J'),
	wulc, wd(107), wurc, nl,
	wel('81'),
	wel('81', 'Choose Bot Level for:'),
	wel('81', Bot),
	wel('81'),
	wsl(107),
	wel('81'), wel('81'),wel('81'),
	wel('81', 'Available levels:'), wel('81'),
	wel('81', 'r. : (Random) random moves'),
	wel('81', 'g. : (Greedy) The bot will choose the next direct best move'),
	wel('81', '<N>. : (Integer) The bot will search the next <N> plays for the best move'),
	wel('81'), wel('81'),wel('81'),
	wllc, wd(107), wlrc, nl, !.

displayWinner(draw):-
    wulc, wd(39), wurc, nl,
	wel('30'), wel('30'),
	wel('30', 'THERE WAS A DRAW'),
	wel('30'), wel('30'),
	wllc, wd(39), wlrc, nl.

displayWinner(Winner):-
    wulc, wd(39), wurc, nl,
	wel('30'),
	wel('30', 'THE WINNER IS:'),
	wel('30'),
	wsl(39),
	wel('30'), wel('30'),
	wel('30', Winner),
	wel('30'), wel('30'),
	wllc, wd(39), wlrc, nl.


% translations
translate(black, 'D').
translate(red, 'R').
translate(ivory, 'I').
translate(green, 'G').
translate(blue, 'B').
translate(wild, 'W').
translate(player1, 'Player1').
translate(player2, 'Player2').
translate(bot, 'BOT').
translate(bot1, 'BOT 1').
translate(bot2, 'BOT 2').

menuTranslate(humanVhuman, 49). % 49 maps to "1"
menuTranslate(humanVbot, 50). % 50 maps to "2"
menuTranslate(botVbot, 51). % 51 maps to "3"
menuTranslate(quit, 52). % 52 maps to "4"
menuTranslate(instructions, 53). % 53 maps to "5"
