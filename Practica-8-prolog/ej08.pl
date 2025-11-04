% Definir el predicado parteQueSuma(+L,+S,-P) que es verdadero cuando P es una lista con elementos de L que
% suman S. Por ejemplo:
% ?- parteQueSuma([1,2,3,4,5],9,P).
% P = [1, 3, 5] ;
% P = [2, 3, 4] ;
% P = [4, 5] ;
% false.

parteQueSuma(_, 0, []).
parteQueSuma([X|L], S, [X|P]) :- S > 0, N is S-X, parteQueSuma(L, N, P).
parteQueSuma([_|L], S, P) :- S > 0, parteQueSuma(L, S, P).