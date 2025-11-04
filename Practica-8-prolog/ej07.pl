% Ejercicio 7 ⋆
% Definir los siguientes predicados, usando member y/o append según sea conveniente:
% i. intersección(+L1, +L2, -L3), tal que L3 es la intersección sin repeticiones de las listas L1 y L2, respe-
% tando en L3 el orden en que aparecen los elementos en L1.


interseccion([], _, []).

% Caso Recursivo 1:
% Si la cabeza (X) de L1 NO es miembro de L2.
% El resultado (L3) es simplemente la intersección del resto de L1 (XS).
interseccion([X|XS], YS, L3_Cola) :-
    not(member(X, YS)),
    interseccion(XS, YS, L3_Cola).

% Caso Recursivo 2:
% Si X SÍ es miembro de L2, Y ADEMÁS X NO está en la intersección del resto.
% Esto cumple el requisito de "sin repeticiones".
% El resultado (L3) es X, seguido de la intersección del resto.
interseccion([X|XS], YS, [X|L3_Cola]) :-
    member(X, YS),
    interseccion(XS, YS, L3_Cola),
    not(member(X, L3_Cola)).

% Caso Recursivo 3:
% Si X SÍ es miembro de L2, PERO X ya estaba en la intersección del resto 
  % (es un duplicado).
  % El resultado (L3) es solo la intersección del resto (ignoramos este X).
  interseccion([X|XS], YS, L3_Cola) :-
      member(X, YS),
      interseccion(XS, YS, L3_Cola),
      member(X, L3_Cola).

% intersección([1,2,3,4,5,6], [1,3,7], [1,3])

% partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. Si L tiene menos de N
% elementos el predicado debe fallar. ¿Cuán reversible es este predicado? Es decir, ¾qué parámetros pueden
% estar indefinidos al momento de la invocación?

% Es reversible gracias a que length y append también lo son

partir(N, L, L1, L2) :- length(L1, N), append(L1, L2, L).

% partir(3, [1,2,3,4,5,6,7], [1,2,3], [4,5,6,7])

% ii. borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista
% ListaOriginal.

% ¿por qué no funciona esto? :(
% borrar(ListaOriginal, X, ListaSinXs) :- member(X, ListaOriginal), not(member(X,ListaSinXs)).


borrar([], _, []).
borrar([X | Cola], X, ListaResultado) :- borrar(Cola, X, ListaResultado).
borrar([H | Cola], X, [H | ListaResultadoCola]) :- H \= X, borrar(Cola, X, ListaResultadoCola).

% iii. sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1

% Caso Base: Si la lista original está vacía, el resultado también.
sacarDuplicados([], []).

% Caso Recursivo:
% 1. Tomamos la cabeza [X | XS].
% 2. CONSERVAMOS X en el resultado: [X | YS].
% 3. Borramos todas las futuras X de la cola (XS) -> XS_Limpia.
% 4. Llamamos recursivamente a sacarDuplicados con la cola limpia.
sacarDuplicados([X | XS], [X | YS]) :- borrar(XS, X, XS_Limpia), sacarDuplicados(XS_Limpia, YS).

% otra versión
sacarDuplicados2([], []).
sacarDuplicados2([X|L1], L2) :- member(X, L1), sacarDuplicados2(L1, L2).
sacarDuplicados2([X|L1], [X|L2]) :- not(member(X, L1)), sacarDuplicados2(L1, L2).

% sacarDuplicados([1,1,2,2,3,3,3,4,5], [1,2,3,4,5])

% iv. permutacion(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. ¿Hay una manera más eficiente
% de definir este predicado para cuando L2 está instanciada?

%! insertar(?X, +L, ?LX)
insertar(X, L, LX) :- append(P,S, L), append(P, [X|S], LX).

% Caso Base: Una forma de insertar X en L es ponerlo al frente.
insertar2(X, L, [X | L]).

% Caso Recursivo: Otra forma es mantener la cabeza H,
% e insertar X recursivamente en la cola T.
insertar2(X, [H | T], [H | R]) :- insertar2(X, T, R).

%! permutacion(+L, ?PS) 
permutacion([], []).
permutacion([L|LS], PS) :- permutacion(LS, P), insertar2(L, P, PS).

% Chequea si L1 es permutación de L2 (ambas deben estar instanciadas)
permutacion_check(L1, L2) :- sort(L1, Sorted_List), sort(L2, Sorted_List).

% v. reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier
% longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.

% Caso Base: Si N=1, el reparto es una lista que contiene a L.
% (N debe ser 1 o mayor, como dice la especificación).
reparto(L, 1, [L]).

% Caso Recursivo: Si N > 1,
% LListas = [L1 | RestoListas] (El resultado es la 1ra lista + el resto)
reparto(L, N, [L1 | RestoListas]) :-
    N > 1,
    % 1. Dividimos L en L1 (la 1ra lista) y L_Resto (lo que queda).
    %    El backtracking de append() probará todas las divisiones 
    %    (incluyendo L1 = []).
    append(L1, L_Resto, L),
    
    % 2. Calculamos el nuevo N (N-1).
    N1 is N - 1,
    
    % 3. Repartimos lo que queda (L_Resto) en N-1 listas.
    reparto(L_Resto, N1, RestoListas).

% vi. repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de LListas puede ser
% vacía, y la longitud de LListas puede variar.

hayVacias(XS) :- member([], XS).

% repartoSinVacias(+L, +N, -LListas)
repartoSinVacias(L1, N, L2) :- reparto(L1, N, L2), not(hayVacias(L2)).


