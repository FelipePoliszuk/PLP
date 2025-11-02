% EJERCICIO 2
vecino(X, Y, [X|[Y|Ls]]).
vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls).

% i. Mostrar el árbol de búsqueda en Prolog para resolver vecino(5, Y, [5,6,5,3]), devolviendo todos los
% valores de Y que hacen que la meta se deduzca lógicamente del programa.

% vecino(5, Y, [5,6,5,3]).
% Y = 6 ;
% Y = 3 ;
% false.


% i. Si se invierte el orden de las reglas, ¿los resultados son los mismos? ¿Y el orden de los resultados?

vecino2(X, Y, [W|Ls]) :- vecino2(X, Y, Ls).
vecino2(X, Y, [X|[Y|Ls]]).

% ?- vecino2(5, Y, [5,6,5,3]).
% Y = 3 ;
% Y = 6.

% El orden de las reglas es crucial en Prolog.

%     Orden Original (Seguro): [Caso Base], [Caso Recursivo]. 
      % Primero resuelve lo que puede probar directamente (el vecino en la primera posición), 
      % dando soluciones de izquierda a derecha en la lista (Y=6 luego Y=3).

%     Orden Invertido: [Caso Recursivo], [Caso Base]. 
      % Primero explora recursivamente (recorre la lista), encontrando el vecino más lejano (Y=3) 
      % antes de regresar y encontrar el más cercano (Y=6).
