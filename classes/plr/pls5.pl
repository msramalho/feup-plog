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
