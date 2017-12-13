:- use_module(library(lists)).
:- use_module(library(clpfd)).
clear:-write('\33\[2J').


main(Colors):-
    % cores: azul-1, verde-2, amarelo-3, preto-4
    % tamanhos: 1 a 4, sendo 1 o menor

	%1 declarar variáveis
    length(Colors, 4),
    length(Sizes, 4),
	%2 declarar domínio
    domain(Colors, 1, 4),
    domain(Sizes, 1, 4),
    %declarar restrições
    all_distinct(Colors),
    all_distinct(Sizes),

    %carro verde está depois do carro azul
    element(Azul, Colors, 1),
    element(Verde, Colors, 2),
    Verde #> Azul,
    %carro amarelo está depois do preto
    element(Amarelo, Colors, 3),
    element(Preto, Colors, 4),
    Amarelo #> Preto,
    %o carro verde é o menor de todos
    element(Verde, Sizes, 1), % 1 é o menor tamanho
    %carro que está imediatamente antes do carro azul é menor do que o que está imediatamente depois do carro azul;
    AntesAzul #= Azul - 1,
    DepoisAzul #= Azul + 1,
    element(AntesAzul, Sizes, TamAntesAzul),
    element(DepoisAzul, Sizes, TamDepoisAzul),
    TamAntesAzul #< TamDepoisAzul,

    %pesquisa de soluções
    labeling([], Colors).

% another car problem:
% 12 carros, 4 amarelos, 2 verdes, 3 vermelhos, 3 azuis
% cor carro 1 = cor ultimo carro
% cor carro 2 = cor penultimo
% 5 é azul
% cada 3 têm cores diferentes
% amarelo-verde-vermelho-azul só aparece uma vez
another(Colors):-
    % amarelo-1, verde-2, vermelho-3, azul-4
    %declarar variáveis
    length(Colors, 12), % 12 carros
    %declarar domínios
    domain(Colors, 1, 4),
    %declarar restrições
    global_cardinality(Colors, [1-4,2-2,3-3,4-3]),%4 amarelos, 2 verdes, 3 vermelhos, 3 azuis
    % cor carro 1 = cor ultimo carro
    element(1, Colors, SameColorFirst),
    element(12, Colors, SameColorFirst),
    % cor carro 2 = cor penultimo
    element(2, Colors, SameColorSecond),
    element(11, Colors, SameColorSecond),
    % 5 é azul
    element(5, Colors, 4),
    % cada 3 têm cores diferentes
    threeDifferent(Colors),
    /* Pos1 #= Pos2 -1,
    Pos2 #= Pos3 -1,
    element(Pos1, Colors, E1),
    element(Pos2, Colors, E2),
    element(Pos3, Colors, E3),
    all_distinct([E1, E2, E3]), */
    % amarelo-verde-vermelho-azul só aparece uma vez
    sequencia(1-2-3-4, Colors, 1),

    %obter variáveis
    labeling([], Colors).

threeDifferent([E1, E2, E3|R]):-
    threeDifferent([E2, E3|R]),
    all_distinct([E1, E2, E3]).
threeDifferent(_).

sequencia(C1-C2-C3-C4, [A, B, C, D|R], Count):-
    sequencia(C1-C2-C3-C4, [B, C, D|R], TempCount),
    (A #= C1 #/\ B #= C2 #/\ C #= C3 #/\ D #= C4) #<=> Cnt,
    Count #= TempCount + Cnt.
sequencia(_, [_], 0).
