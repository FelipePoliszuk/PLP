% Predicados auxiliares

% esPar(+N)
esPar(N) :- N mod 2 =:= 0.

% listaQueSumaN(+N, -L) - genera una lista de numeros que suman N. 
listaQueSumaN(0, []).
listaQueSumaN(N, L) :-
    N > 0,
    between(1, N, N2),
    N3 is N - N2,
    listaQueSumaN(N3, L1),
    append([N2], L1, L).

% subsecuencia(?Sub, +Lista). (Orden Relativo)
subsecuencia([], []).
subsecuencia([X|XS], [X|YS]) :- subsecuencia(XS, YS). % Coinciden (consumo ambos)
subsecuencia(XS, [_|YS]) :- subsecuencia(XS, YS).     % No coinciden (descarto cabeza de Lista)

% transponer(+Matriz, -Transpuesta)
transponer([], []).
transponer([[]|_], []).
transponer(M, [Columna|RestoM]) :-
    sacarPrimeraColumna(M, Columna, RestoFilas),
    transponer(RestoFilas, RestoM).

sacarPrimeraColumna([], [], []).
sacarPrimeraColumna([[H|T]|Filas], [H|Col], [T|Resto]) :-
    sacarPrimeraColumna(Filas, Col, Resto).

% esDivisible(+X, +Y) - verdadero si X es divisible por Y
esDivisible(X, Y) :- X mod Y =:= 0.

% comprimir(+L, -R). Elimina elementos consecutivos iguales.
comprimir([], []).
comprimir([X], [X]).
comprimir([X,X|XS], R) :- comprimir([X|XS], R). % Si son iguales, ignoro el primero
comprimir([X,Y|XS], [X|R]) :- X \= Y, comprimir([Y|XS], R). % Distintos, guardo X

% n_esimo(+K, +Lista, -Elemento) (Índice 0) igual que nth0
n_esimo(0, [X|_], X).
n_esimo(K, [_|XS], R) :- K > 0, K1 is K-1, n_esimo(K1, XS, R).

% maximo(+Lista, -Maximo)
maximo([X], X).
maximo([X|XS], M) :- maximo(XS, M_Resto), M is max(X, M_Resto).

% rango(+A, +B, -L). Genera una lista de numeros desde A hasta B.
rango(A, A, [A]).
rango(A, B, [A|L]) :- A < B, A1 is A + 1, rango(A1, B, L).

% union(+L1, +L2, -R). Union de dos listas. Como append pero sin repetidos.
union([], L, L).
union([X|XS], L2, R) :- member(X, L2), union(XS, L2, R).
union([X|XS], L2, [X|R]) :- not(member(X, L2)), union(XS, L2, R).

% diferencia(+L1, +L2, -R) (Elementos en L1 que no estan en L2)
diferencia([], _, []).
diferencia([X|XS], L2, R) :- member(X, L2), diferencia(XS, L2, R).
diferencia([X|XS], L2, [X|R]) :- not(member(X, L2)), diferencia(XS, L2, R).

% divisores(+N, -L)
divisores(N, L) :- 
    divisoresDesde(1, N, L).

% divisoresDesde(+Actual, +N, -Lista)

divisoresDesde(I, N, []) :- I > N.
divisoresDesde(I, N, [I|Resto]) :- 
    I =< N, 
    0 =:= N mod I, 
    I1 is I + 1, 
    divisoresDesde(I1, N, Resto).
divisoresDesde(I, N, Resto) :- 
    I =< N, 
    0 =\= N mod I, 
    I1 is I + 1, 
    divisoresDesde(I1, N, Resto).

% factorial(?N, ?F)
factorial(0, 1).
factorial(N, F) :- 
    N > 0, 
    N1 is N - 1, 
    factorial(N1, F1), 
    F is N * F1.

% reemplazar(+X, +Y, +L1, -L2) -> Reemplaza todas las X por Y en L1.
reemplazar(_, _, [], []).
reemplazar(X, Y, [X|T], [Y|R]) :- reemplazar(X, Y, T, R). % Coincide, reemplazo
reemplazar(X, Y, [H|T], [H|R]) :- X \= H, reemplazar(X, Y, T, R). % No coincide, sigo

% insertarOrdenado(+Elem, +ListaOrd, -NuevaLista) -> Inserta manteniendo orden.
insertarOrdenado(E, [], [E]).
insertarOrdenado(E, [X|XS], [E,X|XS]) :- E =< X.
insertarOrdenado(E, [X|XS], [X|R]) :- E > X, insertarOrdenado(E, XS, R).

% posicionesPares(+L, -Pares) -> Devuelve elementos en índices 0, 2, 4...
posicionesPares([], []).
posicionesPares([X], [X]).
posicionesPares([X,_|T], [X|R]) :- posicionesPares(T, R). % Salto el segundo (_)    

% Estructura: nil o bin(Izq, Raiz, Der)

% esArbol(+T) - Verifica estructura
esArbol(nil).
esArbol(bin(I, _, D)) :- esArbol(I), esArbol(D).

% preorder(+AB, -L) - genera una lista de los elementos del arbol en preorden
preorder(nil,[]).
preorder(bin(I,R,D),[R|L]) :- preorder(I,LI), preorder(D,LD), append(LI,LD,L).

% nodos(+T, -N) - Cuenta cantidad de nodos
nodos(nil, 0).
nodos(bin(I, _, D), N) :- 
    nodos(I, NI), 
    nodos(D, ND), 
    N is NI + ND + 1.

% altura(+T, -H) - Altura del árbol
altura(nil, 0).
altura(bin(I, _, D), H) :- 
    altura(I, HI), 
    altura(D, HD), 
    H is max(HI, HD) + 1.

% rle(+Lista, -ListaTuplas) cuenta repeticiones de elementos devolviendo tuplas (Elemento, Cantidad).
rle([], []).
rle([X], [(X,1)]).
rle([X,X|Xs], [(X,N)|Ys]) :-  % Caso: Repetido
    rle([X|Xs], [(X,M)|Ys]),  % Recursión sobre la misma cabeza para seguir contando
    N is M + 1.
rle([X,Y|Xs], [(X,1)|Ys]) :-  % Caso: Cambio de elemento
    X \= Y,
    rle([Y|Xs], Ys).          % Reinicio la cuenta con Y    