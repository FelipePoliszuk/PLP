% Definir el predicado aplanar(+Xs, -Ys), que es verdadero sii Ys contiene los elementos de todos los niveles de
% Xs, en el mismo orden de aparición. Los elementos de Xs son enteros, átomos o nuevamente listas, de modo que
% Xs puede tener una profundidad arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.
% Ejemplos:
% ?- aplanar([a, [3, b, []], [2]], L).→ L=[a, 3, b, 2]
% ?- aplanar([[1, [2, 3], [a]], [[[]]]], L).→ L=[1, 2, 3, a]
% Nota: este predicado ya está definido en prolog con el nombre flatten.


aplanar([], []).

% -----------------------------------------------------------------
% Caso Recursivo: aplanar una lista no vacía [Cabeza | Cola].
%
% 1. Aplanamos la Cabeza (recursivamente) -> Ys_Cabeza
% 2. Aplanamos la Cola (recursivamente)   -> Ys_Cola
% 3. El resultado final (Ys) es la concatenación de ambas.
% -----------------------------------------------------------------
aplanar([Cabeza | Cola], Ys) :-
    aplanar(Cabeza, Ys_Cabeza),
    aplanar(Cola, Ys_Cola),
    append(Ys_Cabeza, Ys_Cola, Ys).

% -----------------------------------------------------------------
% Caso Base 2: aplanar un elemento que NO es una lista.
%
% Si X no es una lista (es un átomo o número) Y X no es [].
% La versión "aplanada" de X es simplemente una lista que contiene [X].
%
% Nota: Usamos not(is_list(X)) que está permitido por el ejercicio.
% -----------------------------------------------------------------
aplanar(X, [X]) :- not(is_list(X)), X \= [].
