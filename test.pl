unico(L,U) :- append(Pref,[U|Suf],L), not(member(U,Pref)), not(member(U,Suf)).

%hayUnRepetido(+L)
hayUnRepetido(L) :- member(U,L), not(unico(L,U)).

sinRepetidos(L) :- not(hayUnRepetido(L)).

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

formula(VS,F) :- desde(1,N), formulasDeTamano(VS,F,N).

%formulasDeTamano(+VS,-F,+N)
formulasDeTamano(VS,F,1) :- member(F,VS).
formulasDeTamano(VS,neg(F),N) :- N > 1, N1 is N-1, formulasDeTamano(VS,F,N1).
formulasDeTamano(VS,imp(FP,FQ),N) :- N > 2, N1 is N-1,
    between(1,N1,NP), NQ is N1-NP, 
    formulasDeTamano(VS,FP,NP), formulasDeTamano(VS,FQ,NQ).

% formula/2 no es reversible en F porque desde/2 nunca deja de generar nuevos valores
% para N. Por lo tanto, si bien eventualmente se instancia el tamaño de F en N y se tiene
% éxito en formulasDeTamano/3, el predicado jamás termina.

% funciona bien como  generador, no como verificador (instanciado)


% esRotacion(L,R) :- append(L1,L2,L), append(L2,L1,R), L1 \= [].

esPar(N) :- N mod 2 =:= 0.


% collatz(+N, -S)
collatz(N, N).

collatz(N, S) :-
    N > 1,
    esPar(N), 
    X is N / 2, 
    collatz(X, S).

collatz(N, S) :- 
    N > 1,
    not(esPar(N)), 
    X is (3 * N) + 1, 
    collatz(X, S).


    

% collatzMayor(+N, -M)

collatzMayor(N, M):-
    collatz(N,M),
    not((collatz(N,X),
    X > M
    )).
% -------------------------------------------------------------------
% Ejercicio 2 - Programación Lógica
% Análisis de notas de estudiantes para presentarse a concurso

% Base de datos: predicados disponibles
% estudiante(?E) - verdadero cuando E es un estudiante
% notas(-XS) - instancia XS como lista de triplas (Estudiante,Materia,Nota)

% Datos de ejemplo para pruebas
estudiante(juan).
estudiante(maria).
estudiante(pedro).

notas([
    (juan, plp, 3),
    (juan, plp, 9),
    (juan, tlen, 8),
    (maria, plp, 10),
    (maria, tlen, 9),
    (maria, bd, 8),
    (pedro, plp, 10),
    (pedro, tlen, 9),
    (pedro, bd, 8)
]).

% ============================================================================
% a) tieneMateriaAprobada(+E, +M)
% Verdadero cuando el estudiante E tiene la materia M aprobada (nota >= 4)
% ============================================================================

tieneMateriaAprobada(E, M) :-
    notas(ListaNotas),
    member((E, M, Nota), ListaNotas),
    Nota >= 4.

% ============================================================================
% b) eliminarAplazos(+NS, -L)
% Verdadero cuando NS es una lista de notas y L es la misma lista
% pero eliminando los aplazos. Los aplazos solo se eliminan si el estudiante
% finalmente aprobó la materia.
% ============================================================================

eliminarAplazos(NS, L) :-
    eliminarAplazos_aux(NS, [], L).

% Caso base: lista vacía
eliminarAplazos_aux([], Acc, L) :-
    reverse(Acc, L).

% Procesar cada nota
eliminarAplazos_aux([Nota|Rest], Acc, L) :-
    Nota = (E, M, N),
    (
        % Si es un aprobado (N >= 4), lo incluimos siempre
        N >= 4
        ->
        eliminarAplazos_aux(Rest, [Nota|Acc], L)
        ;
        % Si es un aplazado (N < 4), incluir solo si el estudiante aprobó la materia
        (tieneMateriaAprobada(E, M)
        ->
        eliminarAplazos_aux(Rest, Acc, L)
        ;
        eliminarAplazos_aux(Rest, [Nota|Acc], L)
        )
    ).

% ============================================================================
% c) promedio(+A, -P)
% Verdadero cuando A es un estudiante y P es el promedio de todas sus notas
% luego de eliminar los aplazos
% ============================================================================

promedio(A, P) :-
    estudiante(A),
    notas(ListaNotas),
    % Eliminar aplazos usando la lista de notas del estudiante
    construirListaNotas(A, ListaNotas, NotasAEstudiante),
    eliminarAplazos(NotasAEstudiante, NotasAprobadas),
    % Extraer solo las calificaciones numéricas
    extraerCalificaciones(NotasAprobadas, Calificaciones),
    % Calcular promedio
    Calificaciones \= [],
    sumlist(Calificaciones, Sum),
    length(Calificaciones, Len),
    P is Sum / Len.

% Auxiliar: construir lista de notas para un estudiante
construirListaNotas(A, ListaNotas, NotasDelEstudiante) :-
    findall((A, M, N), member((A, M, N), ListaNotas), NotasDelEstudiante).

% Auxiliar: extraer solo las calificaciones de una lista de triplas
extraerCalificaciones([], []).
extraerCalificaciones([(_,_,N)|Rest], [N|Calificaciones]) :-
    extraerCalificaciones(Rest, Calificaciones).

% ============================================================================
% d) mejorEstudiante(-A)
% Verdadero cuando A es el estudiante cuyo promedio es el más alto.
% Puede haber más de una solución si hay estudiantes con el mismo promedio.
% NO usar estructuras auxiliares como findall/bagof/setof
% ============================================================================

mejorEstudiante(A) :-
    estudiante(A),
    promedio(A, P),
    not((
        estudiante(B),
        promedio(B, PB),
        PB > P
    )).


% subsecuenciaCreciente(+L, -S)

subsecuenciaCreciente(L, S) :-
    subsecuencia(L, S),
    esCreciente(S).

% subsecuencia(+L, -S)
subsecuencia([], []).
subsecuencia([X|XS], [X|YS]) :- subsecuencia(XS, YS).
subsecuencia([_|XS], YS) :- subsecuencia(XS, YS).

% esCreciente(+L)
esCreciente([]).
esCreciente([_]).
esCreciente([X,Y|XS]) :- X < Y, esCreciente([Y|XS]).    


% subsecuenciaCrecienteMasLarga(+L,-S)

subsecuenciaCrecienteMasLarga(L, S):-
    subsecuenciaCreciente(L, S),
    length(S, N),

    not((
        subsecuenciaCreciente(L, Y),
        length(Y, M),
        M > N
    )).




% fibonacci(-X)

fibonacci(X) :- desde(1, M), fibonacci_aux(M, X).

fibonacci_aux(0, 0).
fibonacci_aux(1, 1).
fibonacci_aux(N, X) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci_aux(N1, X1),
    fibonacci_aux(N2, X2),
    X is X1 + X2.   




generarCapicuas(L) :- 
    desde(1, N), 
    listaDeN(N, L), 
    esCapicua(L).

% listaQueSumaN(+N, -L)
listaQueSumaN(0, []).
listaQueSumaN(N, L) :-
    N > 0,
    between(1, N, N2),
    N3 is N - N2,
    listaQueSumaN(N3, L1),
    append([N2], L1, L).

% capicua(+L)
esCapicua(L) :- reverse(L, L).

% otra implementacion de capicua(+L)

capicua2([_]).
capicua2(L) :- 
    append([X],XS,L), 
    append(YS,[X],XS), 
    esCapicua2(YS).

% tokenizar(+D, +F, -T)

tokenizar(_, [], []).
tokenizar(D,F,[X|XS]) :-
    member(X,D),
    append(X,YS, F),
    tokenizar(D,YS,XS).

% mayorCantidadDePalabras(+D, +F, -T)

mayorCantidadDePalabras(D, F, T) :-
    tokenizar(D, F, T),
    length(T, N1),
    not((
        tokenizar(D, F, R),
        length(R, N2),
        N2 > N1 
    )).




movimiento((X,Y), (X,Y1)) :- Y1 is Y+1. % Arriba
movimiento((X,Y), (X,Y1)) :- Y1 is Y-1. % Abajo
movimiento((X,Y), (X1,Y)) :- X1 is X+1. % Derecha
movimiento((X,Y), (X1,Y)) :- X1 is X-1. % Izquierda
    


caminoDesde(P, C) :-
    desde(1, N),           % 1. Generamos una longitud finita: 1, 2, 3...
    caminoDeLargo(N, P, C). % 2. Generamos los caminos de ESA longitud exacta.

% caminoDeLargo(+N, +P, -C)
caminoDeLargo(1, P, [P]).
caminoDeLargo(N, P, [P|Resto]) :-
    N > 1,
    N_Ant is N - 1,
    movimiento(P, Siguiente),       % Damos un paso
    caminoDeLargo(N_Ant, Siguiente, Resto). % Completamos el resto con N-1 pasos    
    




objeto(1,50,10).
objeto(2,75,15).
objeto(3,60,5).
objeto(4,10,1).



mochila(C, L) :-
    mochila_aux(0, C, L).  % Empezamos con ID anterior 0

% --- Predicado Auxiliar ---
% mochila_aux(+IdAnterior, +CapacidadDisponible, -ListaIDs)

mochila_aux(_, _, []).
mochila_aux(Anterior, C, [Id|Resto]) :-
    objeto(Id, P, _),    % 1. Existe un objeto
    Id > Anterior,          % 2. Orden: Solo miramos hacia adelante (evita repetidos y bucles)
    P =< C,      % 3. Restricción: Solo lo agrego si entra (Poda)
    NuevaC is C - P,
    mochila_aux(Id, NuevaC, Resto). % 4. Recursión con nueva capacidad y nuevo ID


% mejorMochila(+C,-L)
mejorMochila(C,L) :-
    mochila(C,L),
    valorMochila(L,N1),
    not((
        mochila(C, R),
        valorMochila(R, N2),
        N2 > N1 
    )).    

% valorMochila(+ListaIDs,-ValorTotal)
valorMochila([],0).
valorMochila([Id|Resto], V) :-
    objeto(Id, _, VObj),
    valorMochila(Resto, VResto),
    V is VObj + VResto. 



preorder(nil,[]).
preorder(bin(I,R,D),[R|L]) :- 
    
    preorder(I,LI), 
    preorder(D,LD),
    append(LI,LD,L).


% append2([],YS,YS).
% append2([X|XS],YS,[X|L]) :- append2(XS,YS,L).