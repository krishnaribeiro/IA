% Definindo locais com coordenadas (x, y)
place(position1, 0, 0).
place(position2, 1, 0).
place(position3, 2, 0).
place(position4, 3, 0).
place(position5, 4, 0).
place(position6, 5, 0).
place(position7, 0, 1).
place(position8, 1, 1).
place(position10, 2, 1).
place(position11, 3, 1).
place(position12, 4, 1).
place(position13, 5, 1).
place(position14, 0, 2).
place(position15, 1, 2).
place(position16, 2, 2).
place(position17, 3, 2).
place(position18, 4, 2).
place(position19, 5, 2).

% Definindo blocos com tamanho horizontal
block(blockA, 1). % BlockA tem tamanho 1
block(blockB, 1). % BlockB tem tamanho 2
block(blockC, 2). % BlockC tem tamanho 3
block(blockD, 3). % BlockD tem tamanho 3

:- dynamic on/2.
:- dynamic clear/2.
:- dynamic clear/1.

on(blockC, (0, 0)).
on(blockC, (1, 0)).
on(blockA, (3, 0)).
on(blockB, (5, 0)).
on(blockD, (3, 1)).
on(blockD, (4, 1)).
on(blockD, (5, 1)).

% Definindo a regra object para incluir blocos com tamanho
object(X) :-
    place(X, _, _);
    block(X, _).

% Definindo quando um local está livre em 2D
clear(X, Y) :-
    place(_, X, Y),
    \+ on(_, (X, Y)).

clear(X) :-
    block(X, _),
    on(X, (X1, Y1)),
    Y2 is Y1+1,
    clear(X1, Y2). % Sempre coloco em cima da esquerda pra direita. Se houver algo em cima do bloco, estará pelo menos na primeira posição.

% Verifica se todas as posições de um bloco estão livres em 2D
vacant(Block, X, Y) :-
    positions(Block, X, Y, Positions),
    find_coordsOns(Block, OnBlock),
    remove_repeated_positions(Positions, OnBlock, FilteredPositions),
    all_clear(FilteredPositions).

find_coordsOns(Block, OnPositions) :-
    findall((X, Y), on(Block, (X, Y)), OnPositions).

remove_repeated_positions([], _, []).
remove_repeated_positions([(X, Y)|Rest], ToFilter, Result) :-
    ( member((X, Y), ToFilter) ->
        remove_repeated_positions(Rest, ToFilter, Result)
    ;
        Result = [(X, Y)|FilteredRest],
        remove_repeated_positions(Rest, ToFilter, FilteredRest)
    ).

% Gera todas as posições ocupadas por um bloco a partir de uma coordenada (X, Y) usando recursão
positions_recursive(_, X, Y, 0, []).
positions_recursive(Block, X, Y, N, [(XPos, Y)|Positions]) :-
    N > 0,
    XPos is X + N - 1,
    N1 is N - 1,
    positions_recursive(Block, X, Y, N1, Positions).

% Usa a função recursiva para gerar todas as posições ocupadas
positions(Block, X, Y, Positions) :-
    block(Block, Size),
    positions_recursive(Block, X, Y, Size, Positions).

% Verifica se todas as posições em uma lista estão livres
all_clear([]).
all_clear([(X, Y)|Rest]) :-
    clear(X, Y),
    all_clear(Rest).

canStack(B1, X1, Y1, B2, X2, Y2) :-
    writeln(['Checking canStack for', B1, 'at', (X1, Y1), 'on', B2, 'at', (X2, Y2)]),
    block(B1, Size1),
    block(B2, Size2),
    on(B2, (Xbase, Ybase)),
    (Size1 =< Size2 -> % Se B1 for maior que B2, pode empilhar diretamente
        writeln(['Block', B1, 'can stack on', B2, 'because its size', Size1, 'is greater than', Size2]);
        (
            Size1 > Size2, % Caso contrário, verifica o equilíbrio
            Diferenca is Size1 - Size2,
            MaxPendurar is ceiling(Size2 / 2),
            (Diferenca =< MaxPendurar ->
                writeln(['Difference', Diferenca, 'is less than or equal to max overhang', MaxPendurar]);
                writeln(['Difference', Diferenca, 'is greater than max overhang', MaxPendurar]), fail),
            (Xbase =:= X2 -> % Verifica se estão na mesma posição x
                writeln(['X positions are the same']);
                writeln(['X positions are different', Xbase, X2]), fail),
            (Y2 =:= Ybase + 1 -> % Verifica se B1 está diretamente acima de B2
                writeln(['Y positions are valid for stacking']);
                writeln(['Y positions are invalid for stacking', Y2, Ybase+1]), fail)
        )
    ).

% Verifica se um movimento é possível em 2D
can(move(B1, (X1, Y1), B2, (X2, Y2))) :-
    writeln(['Checking can move', B1, 'from', (X1, Y1), 'to', (X2, Y2), 'on', B2]),
    block(B1, Size),
    block(B2, Size2),
    ((X2, Y2) \== (X1, Y1) ->
        writeln(['Positions are different']);
        writeln(['Positions are the same']), fail),
    (vacant(B1, X2, Y2) ->
        writeln(['Vacant check passed for', B1, 'at', (X2, Y2)]);
        writeln(['Vacant check failed for', B1, 'at', (X2, Y2)]), fail),
    (canStack(B1, X1, Y1, B2, X2, Y2) ->
        writeln(['Can stack check passed for', B1, 'on', B2]);
        writeln(['Can stack check failed for', B1, 'on', B2]), fail).

% Efetuar o movimento de um bloco de um lugar para outro em 2D
move(B1, (X1, Y1), B2, (X2, Y2)) :-
    writeln(['Attempting to move', B1, 'from', (X1, Y1), 'to', (X2, Y2), 'on', B2]),
    (can(move(B1, (X1, Y1), B2, (X2, Y2))) ->
        writeln(['Move is possible']);
        writeln(['Move is not possible']), fail),
    positions(B1, X1, Y1, OldPositions), % Pega as posições antigas
    writeln(['Old positions for', B1, ':', OldPositions]),
    positions(B1, X2, Y2, NewPositions), % Pega as novas posições
    writeln(['New positions for', B1, ':', NewPositions]),
    % Atualiza os fatos on/2
    find_ons(B1, OldOns),
    retract_all_facts(OldOns),
    writeln(['Retracted all old on facts for', B1]),
    remove_repeated_positions(NewPositions, OldPositions, OnlyNewPositions),
    retract_all_clears(OnlyNewPositions),
    writeln(['Retracted clear facts for old positions', OnlyNewPositions]),
    assert_all_facts(B1, NewPositions),
    writeln(['Asserted new on facts for', B1, ':', NewPositions]),
    remove_repeated_positions(OldPositions, NewPositions, OnlyOldPositions),
    assert_all_clears(OnlyOldPositions),
    writeln(['Asserted clear facts for new positions', OnlyOldPositions]).

% Verifica se um movimento é possível em 2D
can(moveFloor(B1, (X1, Y1), (X2, Y2))) :-
    writeln(['Checking can move', B1, 'from', (X1, Y1), 'to', (X2, Y2), 'on Floor']),
    block(B1, Size),
    ((X2, Y2) \== (X1, Y1) ->
        writeln(['Positions are different']);
        writeln(['Positions are the same']), fail),
    (vacant(B1, X2, Y2) ->
        writeln(['Vacant check passed for', B1, 'at', (X2, Y2)]);
        writeln(['Vacant check failed for', B1, 'at', (X2, Y2)]), fail).

% Efetuar o movimento de um bloco de um lugar para outro em 2D
moveFloor(B1, (X1, Y1), (X2, Y2)) :-
    writeln(['Attempting to move', B1, 'from', (X1, Y1), 'to', (X2, Y2), 'on Floor']),
    (can(moveFloor(B1, (X1, Y1), (X2, Y2))) ->
        writeln(['Move is possible']);
        writeln(['Move is not possible']), fail),
    positions(B1, X1, Y1, OldPositions), % Pega as posições antigas
    writeln(['Old positions for', B1, ':', OldPositions]),
    positions(B1, X2, Y2, NewPositions), % Pega as novas posições
    writeln(['New positions for', B1, ':', NewPositions]),
    find_ons(B1, OldOns),
    retract_all_facts(OldOns),
    writeln(['Retracted all old on facts for', B1]),
    remove_repeated_positions(NewPositions, OldPositions, OnlyNewPositions),
    retract_all_clears(OnlyNewPositions),
    writeln(['Retracted clear facts for old positions', OnlyNewPositions]),
    assert_all_facts(B1, NewPositions),
    writeln(['Asserted new on facts for', B1, ':', NewPositions]),
    remove_repeated_positions(OldPositions, NewPositions, OnlyOldPositions),
    assert_all_clears(OnlyOldPositions),
    writeln(['Asserted clear facts for new positions', OnlyOldPositions]).

find_ons(X, List) :-
    findall(on(X, P), on(X, P), List).

% Remover todos os fatos para um determinado predicado
retract_all_facts([]).
retract_all_facts([Fact | Rest]) :-
    retract(Fact),
    retract_all_facts(Rest).

% Remover todos os fatos clear para uma lista de posições
retract_all_clears([]).
retract_all_clears([(X, Y) | Rest]) :-
    retract(clear(X, Y)),
    retract_all_clears(Rest).

% Adicionar uma lista de fatos clear
assert_all_clears([]).
assert_all_clears([(X, Y) | Rest]) :-
    assertz(clear(X, Y)),
    assert_all_clears(Rest).

% Adicionar uma lista de fatos on
assert_all_facts(_, []).
assert_all_facts(Name, [(X, Y) | Rest]) :-
    assertz(on(Name, (X, Y))),
    assert_all_facts(Name, Rest).

% Função principal para inicializar o estado
init_state :-
    retractall(on(_, _)),
    retractall(clear(_, _)),
    assertz(on(blockC, (0, 0))),
    assertz(on(blockC, (1, 0))),
    assertz(on(blockA, (3, 0))),
    assertz(on(blockB, (5, 0))),
    assertz(on(blockD, (3, 1))),
    assertz(on(blockD, (4, 1))),
    assertz(on(blockD, (5, 1))),
    assertz(clear(2, 0)),
    assertz(clear(4, 0)),
    assertz(clear(0, 1)),
    assertz(clear(1, 1)),
    assertz(clear(2, 1)),
    assertz(clear(0, 2)),
    assertz(clear(1, 2)),
    assertz(clear(2, 2)),
    assertz(clear(3, 2)),
    assertz(clear(4, 2)),
    assertz(clear(5, 2)).

% Verificar o estado após a movimentação
verify_state :-
    findall(on(X, Y), on(X, Y), OnStates),
    findall(clear(X, Y), clear(X, Y), ClearStates),
    write('on states: '), writeln(OnStates),
    write('clear states: '), writeln(ClearStates).

% Inicializar estado e executar a movimentação
execute :-
        init_state,
    move(blockD,(3,1),blockC,(0,1)),
    verify_state,
    move(blockA,(3,0),blockB,(5,1)),
    verify_state,
    moveFloor(blockD,(0,1),(2,0)),
    verify_state,
    move(blockA,(5,1),blockC,(0,1)),
    verify_state,
    move(blockB,(5,0),blockC,(1,1)),
    verify_state,
    moveFloor(blockD,(2,0),(3,0)),
    verify_state,
    move(blockB,(1,1),blockD,(5,1)),
    verify_state,
    move(blockA,(0,1),blockD,(4,1)),
    verify_state,
    move(blockC,(0,0),blockA,(4,2)),
    verify_state.
    