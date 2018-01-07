:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(between)).
clear:-write('\33\[2J').

% 4.1)

numbers(L):-
	L = [X1,X2,X3,X4,X5,X6],
	
	domain(L,1,49),
	all_distinct(L),
	
	L = [X1|Rest],
	cumlist(smaller, Rest, X1, _),
	
	X1 * 2 #= X2,
	X4 #= X2 * 2,
	sum(L, #=, Sum), Sum - X2 #= 100,
	X5 #= X2 + X3,
	X1 + X3 #= X6 - X5,
	
	cumlist(diff, Rest, Diffs, X1, _),
	count(1, Diffs, #=, 1),
	
	labeling([], L).
	
	
smaller(X, Prev, X):- X #>= Prev. 

diff(X, Diff, Prev, X):- Diff #= X - Prev.


% 4.2)

multiplicacao(P, I):-
	P = [P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11],
	numlist(2, 2, 8, _, Dpar),
	list_to_fdset(Dpar, SDpar),
	maplist(inSet(SDpar),P),
	
	I = [I1,I2,I3,I4,I5],
	numlist(1, 2, 9, _, Dimpar),
	list_to_fdset(Dimpar, SDimpar),
	maplist(inSet(SDimpar),I),
	
	First #= 100 * I1 + 10 * P1 + P2,
	Second #= 10 * P3 + P4,
	Third #= 1000 * P5 + 100 * I2 + 10 * P6 + P7,
	Fourth #= 100 * P8 + 10 * I3 + P9,
	Last #= 1000 * I4 + 100 * I5 + 10 * P10 + P11,
	
	First * Second #= Last,
	P4 * First #= Third,
	P3 * First #= Fourth,
	Third + 10 * Fourth #= Last,
	
	append(P, I, Vars),
	labeling([], Vars).
	
inSet(Set, Val):- Val in_set Set.


% 4.3)

% fabrica(Id, PodeProduzir, )
% produto(Id, CustoPorFabrica, Preco, (Min, Max))

%devolve matrix em que linhas são as fabricas e colunas os produtos, o valor numa célula é a quantidade desse produto a ser produzido nessa fábrica
% há um erro na parte das fábricas que não produzem dado produto
calculo(ProducaoPorFabrica, Rendimento):-
	findall([Id, CPF, Preco, PRange],(
		produtos(Id, CPF, Preco, (Min, Max)), 
		numlist(Min, Max, ListP),
		list_to_fdset(ListP, PRange)
	),Produtos),
	findall([Id, PodeProduzir],
		fabrica(Id, PodeProduzir)
		% findall(Pid-Range, (
			% member(Pid-(Minimo,Maximo), PodeProduzir),
			% numlist(Minimo, Maximo, ListF),
			% list_to_fdset(ListF, Range)
		% ),PP)
	,Fabricas),
	
	% variables
	%criar matriz com as dimensoes certas
	same_length(ProducaoPorFabrica, Fabricas),
	maplist(same_length(Produtos), ProducaoPorFabrica),
		
	% constraints
	maplist(getMachine(ProducaoPorFabrica), Fabricas, Machines),
	
	transpose(ProducaoPorFabrica, FabricaPorProduto),
	maplist(getTasks(FabricaPorProduto), Produtos, Tasks),
	append(Tasks, AllTasks),
	cumulatives(AllTasks, [machine(1000, 9999)|Machines], [bound(upper)]),
	
	% labeling
	append(ProducaoPorFabrica, Vars),
	cumlist(getLucro(FabricaPorProduto), Produtos, 0, Lucros),
	sum(Lucros, #=, Rendimento),
	write('labeling...\n'),
	labeling([maximize(Rendimento),ffc,down,time_out(1000,_)], Vars).

getMachine(ProducaoPorFabrica, [Id, _], machine(Id, Total)):-
	nth1(Id, ProducaoPorFabrica, Fabrica),
	Total in 1..9000,
	sum(Fabrica, #=, Total).

getTasks(FabricaPorProduto, [Id, CPF, _Preco, PRange], Tasks):-
	nth1(Id, FabricaPorProduto, Produto),
	domain(Produto, 0, 9000),
	sum(Produto, #=, Total),
	Total in_set PRange,
	cumlist(getTask(Id), CPF, Tasks, Costs, 1, _),
	sum(Costs, #=, Total).

getTask(_, 0, task(0, 0, 0, 0, 1000), 0, Mid, NewAcc):- !, NewAcc #= Mid + 1.
getTask(Id, Custo, task(0, D, D, Custo, Mid), D, Mid, NewAcc):- 
	NewAcc #= Mid + 1,
	fabrica(Mid, L),
	(member(Id-(Min,Max), L), D in Min..Max);
	(member(Id-Val, L), D #= Val).
	
	
getLucro(FabricaPorProduto, [Id, CPF, Preco, _], _, Lucro):-
	nth1(Id, FabricaPorProduto, Quantidades),
	sum(Quantidades, #=, ContagemTotal), ContagemTotal * Preco #= Bruto,
	cumlist(custos, CPF, Quantidades, 0, LCustos),
	sum(LCustos, #=, Custos),
	Lucro #= Bruto - Custos.

custos(Custo, Quantidade, _, Custos):- Custos #= Custo * Quantidade.
	
	
fabricas(3).
produtos(6).
fabrica(1,[2-(20,100), 3-20, 4-(20,30), 5-50]). % fábrica 1 não pode produzir produto 1, pode produzir 20 a 100 unidades do produto 2, produz 20 do produto 3, porduz 20 a 30 do 4, 50 do 5 e não pode produzir o 6.
fabrica(2,[1-(0,100), 3-(0,20), 4-(10,20), 5-(20,50), 6-(10,50)]).
fabrica(3,[1-(0,100), 2-(50,100), 3-(0,20), 4-(10,20), 5-(20,50), 6-(10,50)]).
produtos(1,[0,5,3],10,(20,150)). %produto 1 custa 5 a ser produzido na fabrica 2 e custa 3 a ser produzido na fábrica 3. Cada unidade vende-se por 10, a quantidade minima a produzir é 20 e a máxima é 150.
produtos(2,[7,4,5],13,(10,100)).
produtos(3,[2,2,1],5,(60,250)).
produtos(4,[5,0,3],8,(30,80)).
produtos(5,[1,3,2],5,(80,350)).
produtos(6,[10,12,0],20,(10,80)). 










