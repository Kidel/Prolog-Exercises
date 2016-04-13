%% maxlist (trova il massimo elemento di una lista)

%% maxlist([A],A).
maxlist([X|Rest], M) :- maxlist(Rest, M), M>=X, !.
maxlist([X|_],X). %% è un "altrimenti" a causa del cat di prima


%% oppure

max(X,Y,X) :- X>=Y, !.
max(_,Y,Y).

maxlist2([X],X) :- !.
maxlist2([X|Coda], N) :-
		maxlist2(Coda,H), max(X,H,N).
		
%% oppure (implementando l'algoritmo classico col ciclo)

maxlist3([X|Coda], N) :-
		ciclo(Coda, X, N).

ciclo([], X, X). %% caso base = uscita dal ciclo
ciclo([Y|Coda], X, N) :- 
		Y>X, !, ciclo(Coda, Y, N).
ciclo([Y|Coda], X, N) :- 
		X>=Y, ciclo(Coda, X, N).
		

%% subset
subset([],_).
subset([X|Tail], Set) :- member(X, Set), subset(Tail, Set).

%% prefisso
prefisso(Pre, L) :- append(Pre, _, L).

%% sublist (i prefissi di L sono sublist, ma anche i prefissi della coda, della coda della coda, ecc)
sublist(Sub, List) :- prefisso(Sub, List).
sublist(Sub, [_|List]) :- sublist(Sub, List).

sublist2(Sub, List) :- append(_, Sub, X), append(X, _, List).

%% split divide i pari e i dispari in 2 liste e i dispari sono scritti al contrario (usare append per i dispari)
pari(N) :- 0 is (N mod 2).

split([], [], []).
split([X|Coda], [X|P], D) :- pari(X), !, split(Coda, P, D).
split([X|Coda], P, Y) :- split(Coda, P, D), !, append(D, [X], Y).



%% esercizio sui grafi

arc(a,b).
arc(a,e).
arc(b,a).
arc(b,c).
arc(c,c).
arc(c,d).
arc(d,c).
arc(d,b).
arc(e,c).

%% definire un predicato path che trovi un cammino da start a goal.

path(Start, Goal, Path) :- path(Start, Goal, Path, []).

path(Start, Start, [Start], Visited) :- \+ member(Start, Visited).
path(Start, Goal, [Start|Path], Visited) :-
		\+ member(Start, Visited), 
		arc(Start, N), 
		path(N, Goal, Path, [Start|Visited]).

		
		
%% mkset
mkset([],[]).
mkset([X|Rest],[X|Set]):- \+ member(X,Rest), !,mkset(Rest,Set).
mkset([_|Rest],Set) :- mkset(Rest,Set).


