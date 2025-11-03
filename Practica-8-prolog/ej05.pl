% Definir los siguientes predicados sobre listas usando append:

% i. last(?L, ?U), donde U es el último elemento de la lista L.

% last con append
last(L, U) :- append(_, [U], L).

% last sin append
last_recursivo([U], U). 
last_recursivo([_ | L], U) :- last_recursivo(L, U).

% ii. reverse(+L, ?R), donde R contiene los mismos elementos que L, pero en orden inverso.
% Ejemplo: reverse([a,b,c], [c,b,a]).

reverse([], []). 
reverse([X|L], R) :- reverse(L, Lrec), append(Lrec, [X], R).

% iii. prefijo(?P, +L), donde P es prefijo de la lista L

prefijo(P, L) :- append(P, _, L).

% iv. sufijo(?S, +L), donde S es sufijo de la lista L.

sufijo(S, L) :- append(_, S, L).

% v. sublista(?S, +L), donde S es sublista de L.

sublista(S, L) :- append(_, Resto, L), append(S, _, Resto).

% vi. pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L. (Este predicado ya viene
% definido en Prolog y se llama member).

% pertenece con append

pertenece(X, L) :- append(_, [X | _], L).


% pertenece sin append
pertenece2(X, [X | _]).
pertenece2(X, [_ | L]) :- pertenece2(X, L).

