natural(0).
natural(suc(X)) :- natural(X).

% menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
% menorOIgual(X,X) :- natural(X).


% i. Explicar qué sucede al realizar la consulta menorOIgual(0,X).

% Se cuelga en un bucle infinito por la primera clausula, crea una numero infinitamente grande.

% Describir las circunstancias en las que puede colgarse un programa en Prolog. Es decir, ejecutarse infini-
% tamente sin arrojar soluciones.

% Un programa Prolog se cuelga cuando el motor de búsqueda, siguiendo el orden de las cláusulas (de arriba abajo)
% y el orden de los literales dentro de la cláusula (de izquierda a derecha), se encuentra con un camino recursivo 
% que nunca lo lleva a un hecho o a una condición de fallo que pueda terminar el proceso. El orden de las cláusulas 
% y sus literales influyen directamente en el resultado y la terminación del programa.

% iii. Corregir la defición de menorOIgual para que funcione adecuadamente.


menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

% anotacion: usar suc(x), no funciona con naturales literal como: 1,2,3...
