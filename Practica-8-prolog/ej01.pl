padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).


% i. ¿Cuál el resultado de la consulta abuelo(X, manuel)?
% El  resultado es " X = juan"

% ii. A partir del predicado binario padre, definir en Prolog los predicados binarios: hijo, hermano y
% descendiente.

hijo(X,Y) :- padre(Y,X).

hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.

descendiente(X,Y) :- padre(Y,X).
descendiente(X,Y) :- padre(Y,Z), descendiente(X,Z).


% iii. Dibujar el árbol de búsqueda de Prolog para la consulta descendiente(Alguien, juan).

% [NIVEL 0: El Objetivo Inicial]
% ?- descendiente(Alguien, juan).
%  |
%  +-- [RAMA 1: Intenta Cláusula 1]
%  |   (Unifica X=Alguien, Y=juan)
%  |   ?- padre(juan, Alguien).
%  |   |
%  |   +-- Busca Hechos: padre(juan, carlos).
%  |   |   -> ÉXITO 1: Alguien = carlos
%  |   |
%  |   +-- (Backtrack): Busca Hechos: padre(juan, luis).
%  |   |   -> ÉXITO 2: Alguien = luis
%  |   |
%  |   +-- (Backtrack): Busca Hechos: (No hay más)
%  |       -> Falla (esta sub-rama termina)
%  |
%  +-- [RAMA 2: (Backtrack): Intenta Cláusula 2]
%      (Unifica X=Alguien, Y=juan)
%      ?- padre(juan, Z), descendiente(Alguien, Z).
%      |
%      +-- Resuelve 1er goal: padre(juan, Z)
%      |   (Encuentra Z=carlos)
%      |   ?- descendiente(Alguien, carlos).   <-- [INICIO RECURSIÓN 1]
%      |   |
%      |   +-- [RAMA 2.1: Intenta Cláusula 1]
%      |   |   (Unifica X=Alguien, Y=carlos)
%      |   |   ?- padre(carlos, Alguien).
%      |   |   |
%      |   |   +-- Busca Hechos: padre(carlos, daniel).
%      |   |   |   -> ÉXITO 3: Alguien = daniel
%      |   |   |
%      |   |   +-- (Backtrack): Busca Hechos: padre(carlos, diego).
%      |   |   |   -> ÉXITO 4: Alguien = diego
%      |   |   |
%      |   |   +-- (Backtrack): Busca Hechos: (No hay más)
%      |   |       -> Falla
%      |   |
%      |   +-- [RAMA 2.2: (Backtrack): Intenta Cláusula 2]
%      |   |   (Unifica X=Alguien, Y=carlos)
%      |   |   ?- padre(carlos, Z2), descendiente(Alguien, Z2).
%      |   |   |
%      |   |   +-- Resuelve 1er goal: padre(carlos, Z2)
%      |   |   |   (Encuentra Z2=daniel)
%      |   |   |   ?- descendiente(Alguien, daniel).
%      |   |   |   |
%      |   |   |   +-- (Intenta Cláusula 1): ?- padre(daniel, Alguien). -> Falla
%      |   |   |   +-- (Intenta Cláusula 2): ?- padre(daniel, Z3), ... -> Falla
%      |   |   |
%      |   |   +-- (Backtrack): Resuelve 1er goal: padre(carlos, Z2)
%      |   |   |   (Encuentra Z2=diego)
%      |   |   |   ?- descendiente(Alguien, diego).
%      |   |   |   |
%      |   |   |   +-- (Intenta Cláusula 1): ?- padre(diego, Alguien). -> Falla
%      |   |   |   +-- (Intenta Cláusula 2): ?- padre(diego, Z3), ... -> Falla
%      |   |   |
%      |   |   +-- (Backtrack): (No hay más padres de carlos)
%      |   |       -> Falla (esta sub-rama termina)
%      |   |
%      |   +-- (Ambas ramas de 'descendiente(Alguien, carlos)' fallaron)
%      |
%      +-- (Backtrack): Resuelve 1er goal: padre(juan, Z)
%      |   (Encuentra Z=luis)
%      |   ?- descendiente(Alguien, luis).   <-- [INICIO RECURSIÓN 2]
%      |   |
%      |   +-- [RAMA 2.3: Intenta Cláusula 1]
%      |   |   (Unifica X=Alguien, Y=luis)
%      |   |   ?- padre(luis, Alguien).
%      |   |   |
%      |   |   +-- Busca Hechos: padre(luis, pablo).
%      |   |   |   -> ÉXITO 5: Alguien = pablo
%      |   |   |
%      |   |   +-- (Backtrack): Busca Hechos: padre(luis, manuel).
%      |   |   |   -> ÉXITO 6: Alguien = manuel
%      |   |   |
%      |   |   +-- (Backtrack): Busca Hechos: padre(luis, ramiro).
%      |   |   |   -> ÉXITO 7: Alguien = ramiro
%      |   |   |
%      |   |   +-- (Backtrack): (No hay más)
%      |   |       -> Falla
%      |   |
%      |   +-- [RAMA 2.4: (Backtrack): Intenta Cláusula 2]
%      |   |   (Unifica X=Alguien, Y=luis)
%      |   |   ?- padre(luis, Z2), descendiente(Alguien, Z2).
%      |   |   |
%      |   |   +-- (Prueba Z2=pablo) -> ?- descendiente(Alguien, pablo). -> Falla
%      |   |   +-- (Prueba Z2=manuel) -> ?- descendiente(Alguien, manuel). -> Falla
%      |   |   +-- (Prueba Z2=ramiro) -> ?- descendiente(Alguien, ramiro). -> Falla
%      |   |   |
%      |   |   +-- (Backtrack): (No hay más padres de luis)
%      |   |       -> Falla (esta sub-rama termina)
%      |   |
%      |   +-- (Ambas ramas de 'descendiente(Alguien, luis)' fallaron)
%      |
%      +-- (Backtrack): (No hay más padres de juan)
%          -> Falla (esta rama termina)
%  |
% (FIN DE LA BÚSQUEDA)


% iv. ¿Qué consulta habría que hacer para encontrar a los nietos de juan?

% Habría que hacer "?- abuelo(juan, X)." o también se puede hacer "?- hijo(Z, juan), hijo(X, Z)." obteniendo los hijos en la variable X.

% v. ¿Cómo se puede definir una consulta para conocer a todos los hermanos de pablo?

% Habría que hacer "?- hermano(X,pablo)"

% vi. Considerar el agregado del siguiente hecho y regla y la base de conocimiento del ítem anterior.

% ancestro(X, X).
% ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% vii. Explicar la respuesta a la consulta ancestro(juan, X). ¾Qué sucede si se pide más de un resultado?
% ?- ancestro(juan, X).
% X = juan ;
% X = carlos ;
% X = luis ;
% X = daniel ;
% X = diego ;
% X = pablo ;
% X = manuel ;
% X = ramiro ;

% Por el orden de los objetivos se termina colgando el programa, busca recursivamente ancestro(Z, Y)

% viii. Sugerir una solución al problema hallado en los puntos anteriores reescribiendo el programa de ancestro.

  ancestro(X, X).
  ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y). 
