% Predicados guia

% append2(?Lista1, ?Lista2, ?Lista3)

append2([], YS, YS).
append2([X|XS], YS, [X|L]) :- append2(XS, YS, L).

% last(?L, ?U), donde U es el último elemento de la lista L.

last([U], U).
last([_|L], U) :- last(L, U).

% reverse(?L, ?R), donde R contiene los mismos elementos que L, pero en orden inverso.

reverse([], []).
reverse([X|L], R) :- reverse(L, Lrec), append(Lrec, [X], R).

% prefijo(?P, +L), donde P es prefijo de la lista L.

prefijo(P, L) :- append(P, _, L).

% sufijo(?S, +L), donde S es sufijo de la lista L.

sufijo(S, L) :- append(_, S, L).

% sublista(?S, +L), donde S es sublista de L. Segmento Contiguo)

sublista(S, L) :- append(_, Resto, L), append(S, _, Resto).

% pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L.

pertenece(X, L) :- append(_, [X | _], L).

% otra version de pertenece
pertenece2(X, [X | _]).
pertenece2(X, [_ | L]) :- pertenece2(X, L).

% aplanar(+Xs, -Ys) - este predicado ya está definido en prolog con el nombre flatten.

aplanar([], []).
aplanar([Cabeza | Cola], Ys) :-
    aplanar(Cabeza, Ys_Cabeza),
    aplanar(Cola, Ys_Cola),
    append(Ys_Cabeza, Ys_Cola, Ys).
aplanar(X, [X]) :- not(is_list(X)), X \= [].

% intersección(+L1, +L2, -L3) 

interseccion([], _, []).

interseccion([X|XS], YS, L3_Cola) :-
    not(member(X, YS)),
    interseccion(XS, YS, L3_Cola).

interseccion([X|XS], YS, [X|L3_Cola]) :-
    member(X, YS),
    interseccion(XS, YS, L3_Cola),
    not(member(X, L3_Cola)).
interseccion([X|XS], YS, L3_Cola) :-
    member(X, YS),
    interseccion(XS, YS, L3_Cola),
    member(X, L3_Cola).

% partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. 

partir(N, L, L1, L2) :- length(L1, N), append(L1, L2, L).

% ii. borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista
% ListaOriginal.

borrar([], _, []).
borrar([X | Cola], X, ListaResultado) :- borrar(Cola, X, ListaResultado).
borrar([H | Cola], X, [H | ListaResultadoCola]) :- H \= X, borrar(Cola, X, ListaResultadoCola).

% iii. sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1. Se queda con la primera aparición

sacarDuplicados([], []).
sacarDuplicados([X | XS], [X | YS]) :- borrar(XS, X, XS_Limpia), sacarDuplicados(XS_Limpia, YS).

% otra versión de sacarDuplicados. se queda con la ultima aparición 
sacarDuplicados2([], []).
sacarDuplicados2([X|L1], L2) :- member(X, L1), sacarDuplicados2(L1, L2).
sacarDuplicados2([X|L1], [X|L2]) :- not(member(X, L1)), sacarDuplicados2(L1, L2).

% permutación(+L1, ?L2)

permutacion([], []).
permutacion([L|LS], PS) :- permutacion(LS, P), insertar(L, P, PS).

% insertar(?X, +L, ?LX)

insertar(X, L, LX) :- append(P,S, L), append(P, [X|S], LX).

% otra implementacion de insertar

insertar2(X, L, [X | L]).
insertar2(X, [H | T], [H | R]) :- insertar2(X, T, R).

% reparto(+L, +N, -LListas). 

reparto(L, N, LListas) :- reparto_aux(L, N, [], LListas).

reparto_aux(_, 0, LListas, LListas).
reparto_aux(L, N, LListas_Actual, LListas) :-
    partir(N, L, L1, L2),
    reparto_aux(L2, N, [L1|LListas_Actual], LListas).

% vi. repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de LListas puede ser
% vacía, y la longitud de LListas puede variar.

hayVacias(XS) :- member([], XS).

% repartoSinVacias(+L, +N, -LListas)
repartoSinVacias(L1, N, L2) :- reparto(L1, N, L2), not(hayVacias(L2)).


% Definir el predicado parteQueSuma(+L,+S,-P) que es verdadero cuando P es una lista con elementos de L que
% suman S. Por ejemplo:

parteQueSuma(_, 0, []).
parteQueSuma([X|L], S, [X|P]) :- S > 0, N is S-X, parteQueSuma(L, N, P).
parteQueSuma([_|L], S, P) :- S > 0, parteQueSuma(L, S, P).

% desde(+X, -Y)
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% desdeReversible(+X, ?Y)

desdeReversible(X, Y) :- nonvar(Y), Y >= X.
desdeReversible(X, Y) :- var(Y), desde(X, Y).

% intercalar(?L1, ?L2, ?L3)
intercalar([], L, L) :- L \= [].
intercalar(L, [], L).
intercalar([X|L1], [Y|L2], [X|[Y|L3]]) :- intercalar(L1, L2, L3).

% ítem I
% inOrder(+AB, -Lista)
inOrder(nil, []).
inOrder(bin(I, R, D), L) :- inOrder(I, LI), inOrder(D, LD), append(LI, [R], A1), append(A1, LD, L). 

% ítem II
% árbolConInorder(+Lista, -AB)
arbolConInorder([], nil).
arbolConInorder(XS, AB) :- append(LI, [X|LD], XS), arbolConInorder(LI, AI), arbolConInorder(LD, AD), AB = bin(AI, X, AD).

% ítem III
% aBB(+T)
aBB(nil).
aBB(B) :- inOrder(B, L), msort(L, L).

% ítem IV
% insertar(+E, +A1, -A2).
insertarAB(E, nil, bin(nil, E, nil)).
insertarAB(E, bin(BI, R, BD), bin(A2, R, BD)) :- insertarAB(E, BI, A2).
insertarAB(E, bin(BI, R, BD), bin(BI, R, A2)) :- insertarAB(E, BD, A2).

% aBBInsertar(+X, +T1, -T2); ¿es reversible en alguno de sus parámetros?
aBBInsertar(X, T1, T2) :- insertarAB(X, T1, T2), aBB(T2).

% coprimos(-X,-Y)
coprimos(X, Y) :-
    desde(1, X),        % 1. Genera X: 1, 2, 3... 
    between(1, X, Y),   % 2. Genera Y: 1..X 
    1 =:= gcd(X, Y).    % 3. Testea coprimalidad

% otra version de coprimos
coprimos2(X, Y) :- between(1, inf, X), between(1, X, Y), 1 is gcd(X, Y).

% listaQueSuma(+S, +L, -XS)
% S = número que tiene que sumar
% L = largo de la lista
% XS = lista
listaQueSuma(0, 0, []).
listaQueSuma(S, L, [X|XS]) :- L > 0, between(0, S, X), S1 is S-X, L1 is L-1, listaQueSuma(S1, L1, XS).

% matrizQueSuma(+S, +F, +C, -XS)
% S = lo que tiene que sumar cada lista
% F = cantidad de listas (filas)
% C = largo de las listas (columnas)
% XS = matriz
matrizQueSuma(_, 0, _, []).
matrizQueSuma(S, F, C, [X|XS]) :- F > 0, listaQueSuma(S, C, X), F1 is F-1, matrizQueSuma(S, F1, C, XS).

% cuadradoSemiMágico(+N, -XS)
cuadradoSemiMagico(N, XS) :- between(0, inf, M), matrizQueSuma(M, N, N, XS).
% instancio la cantidad de filas y de columnas con el mismo número (el tamaño del
% cuadrado, o sea N) porque quiero justamente que sea un cuadrado. con between
% genero naturales y con eso genero cuadrados semimágicos.

% tri(A,B,C).

% ítem I
% esTriangulo(+T)
esTriangulo(tri(A,B,C)) :- 0<A, 0<B, 0<C, A < B+C, B < A+C, C < B+A.

tupla((A,B)) :- between(0,inf,N), between(0,N,A), B is N-A.

% ítem II
% perimetro(?T, ?P)
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), P is A+B+C.
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), nonvar(P), 
    between(1,P,A), between(1,P,B), between(1,P,C), P is A+B+C, 
    esTriangulo(tri(A,B,C)).
perimetro(tri(A,B,C), P) :- not(ground((tri(A,B,C)))), var(P), between(1,inf,P), perimetro(tri(A,B,C), P).

% ítem III
% triangulo(-T)
triangulo(T) :- perimetro(T, _).

% esCorte(+L,-L1,-L2)
esCorte(L, [X|XS], [Y|YS]) :- append([X|XS], [Y|YS], L).

% esMejorCorte(+C1, C2)
esMejorCorte(C1, C2, D1, D2) :-
    sumlist(C1, SC1),
    sumlist(C2, SC2),
    S1 is abs(SC1-SC2),
    sumlist(D1, SD1),
    sumlist(D2, SD2),
    S2 is abs(SD1-SD2),
    S1 < S2.

% corteMásParejo(+L,-L1,-L2)
corteMasParejo(L, L1, L2) :-
    esCorte(L, L1, L2),
    not((esCorte(L, M1, M2), esMejorCorte(M1, M2, L1, L2))).

% esPrimo(+N)
esPrimo(2).
esPrimo(N) :- N>2, M is N-1, not((between(2, M, B), mod(N, B) =:= 0)).

% numeroPoderoso(+M)
numeroPoderoso(M) :- not((between(2, M, P), esPrimo(P), 
    mod(M,P) =:= 0, B2 is P*P, mod(M,B2) =\= 0)).

% minimoDesde(-X, +C)
minimoDesde(C, C) :- numeroPoderoso(C).
minimoDesde(X, C) :- not(numeroPoderoso(C)), C1 is C+1, minimoDesde(X, C1).

% próximoNumPoderoso(+X,-Y)
proximoNumPoderoso(X, Y) :- X1 is X+1, minimoDesde(Y, X1).

% arbol(+A)
arbol(nil).
arbol(bin(I, _, D)) :-
    arbol(I),
    arbol(D).

% nodosEn(?A, +L)
nodosEn(nil, _).
nodosEn(bin(I, R, D), L) :- member(R, L), nodosEn(I, L), nodosEn(D, L).

% arbolSinRepEn(-A, +L)
arbolSinRepEn(A, L) :-
    arbolSinRepEn_aux(A, L, _Resto).

arbolSinRepEn_aux(nil, L, L).
arbolSinRepEn_aux(bin(I, R, D), L0, L3) :-
    select(R, L0, L1),              % elige una etiqueta disponible y la saca de la lista
    arbolSinRepEn_aux(I, L1, L2),   % reparte las etiquetas restantes en el subárbol izquierdo
    arbolSinRepEn_aux(D, L2, L3).   % y en el derecho

sinRepEn(A, L) :- arbolSinRepEn(A, L).
    