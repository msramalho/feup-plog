:- use_module(library(lists)).
clear:-write('\33\[2J').

esvaziar([Valor, Capacidade], [0, Capacidade]):-Valor>0.%esvaziar([3,3], Balde).
encher([Valor, Capacidade], [Capacidade, Capacidade]):-Valor<Capacidade.%encher([0,5], Balde).
despejarAteEncher([Vde,Cde], [Vpara, Cpara], [VNde, Cde], [Cpara, Cpara]):-%as capacidades mantêm-se e enche logo o PARA
	Falta is Cpara - Vpara, %ver quanto falta no balde PARA
	Vde >= Falta, %o balde DE tem de ter suficiente para encher PARA
	VNde is Vde - Falta. %tirar do balde inicial
	
despejarAteEsvaziar([Vde,Cde], [Vpara, Cpara], [0, Cde], [VNpara, Cpara]):-%as capacidades mantêm-se e esvazia logo o de origem
	Vde > 0,
	VNparaHipotetico is Vpara + Vde,
	min_member(VNpara, [VNparaHipotetico, Cpara]).

%operation on Balde
/* opBalde(B1:_, B2, B3:esvaziar, B2):-esvaziar(B1, B3).
opBalde(B1:_, B2, B3:encher, B2):-encher(B1, B3).
opBalde(B1:_, B2:_, B3:despejarAteEncherOrigem, B4:despejarAteEncherDestino):-despejarAteEncher(B1, B2, B3, B4).
opBalde(B1:_, B2:_, B3:despejarAteEsvaziarOrigem, B4:despejarAteEsvaziarDestino):-despejarAteEsvaziar(B1, B2, B3, B4). */

opBalde(B1, B2, B3, B2):-esvaziar(B1, B3).
opBalde(B1, B2, B3, B2):-encher(B1, B3).
opBalde(B1, B2, B3, B4):-despejarAteEncher(B1, B2, B3, B4).
opBalde(B1, B2, B3, B4):-despejarAteEsvaziar(B1, B2, B3, B4).

%get all buckets in the next level
obterProximos([], Acc, Final):-remove_dups(Acc, Final).
obterProximos([Bi-Bf|T], Acc, Proximos):-
	findall(BaldeI1-BaldeF1, opBalde(Bi, Bf, BaldeI1, BaldeF1), Temp1),
	findall(BaldeI2-BaldeF2, opBalde(Bf, Bi, BaldeI2, BaldeF2), Temp2),
	append([Temp1, Temp2, Acc], TempProximos),
	obterProximos(T, TempProximos, Proximos).
	
%algumFinal
algumFinal([BObjetivo-_|_], BObjetivo).%first is final
%algumFinal([_-BObjetivo|_], BObjetivo).%second is final, not need according to the text
algumFinal([_|T], BObjetivo):-algumFinal(T, BObjetivo).%none go to next, fails on empty list
	
bfs(Baldes, BObjetivoopBalde):-algumFinal(Baldes, BObjetivoopBalde), !.
bfs(Baldes, BObjetivoopBalde):-proximaOperacao(Baldes, BObjetivoopBalde).

proximaOperacao([BObjetivo-_], BObjetivo).
proximaOperacao(Baldes, BObjetivoopBalde):-
	obterProximos(Baldes, [], Proximos),
	write('Proximos: '), write(Proximos), nl, 
	bfs(Proximos, BObjetivoopBalde).
	
calcula:-
	B1 = [0, 4],
	B2 = [0, 3],
	proximaOperacao([B1-B2], [2,_]).
	
	
	
	
	
	
	
	
	
	
	