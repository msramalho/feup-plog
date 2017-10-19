wulc:-put_code(9484). % write upper left corner ┌
wurc:-put_code(9488). % write upper right corner ┐
wllc:-put_code(9492). % write lower left corner ┘
wlrc:-put_code(9496). % write lower right corner └
wlf:-put_code(9500). % write left fork ├
wrf:-put_code(9508). % write right fork ┤
wvd:-put_code(9474). % write vertical dash│

wd:-put_code(9472). % write horizontal dash ─
wd(0):-wd.
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
	wsl(40),
	wel('30'), wel('30'),wel('30'),
	wel('30', '1: Start Game'),
	wel('30', '2: See operations'),
	wel('30', '3: Quit'),
	wel('30'), wel('30'),wel('30'),
	wllc, wd(39), wlrc.

