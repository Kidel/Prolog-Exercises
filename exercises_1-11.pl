%% fact(+X,?Y), vero se Y è il fattoriale di X.

fact(0, 1) :- !.
fact(X, Y) :-  X>0, Z is (X-1), fact(Z, K), Y is (K*X), !.


%% palindroma(X)

palindroma([]) :- !.
palindroma([_]) :- !.
palindroma([X|Rest]) :- append(Y,[X],Rest), palindroma(Y), !.


%% maxlist(+L,?N)

max(X,Y,X) :- X >= Y, !.
max(_,Y,Y).

maxlist([],_) :- fail.
maxlist([X],X) :- !.
maxlist([X|Rest], N) :- maxlist(Rest, M), max(X, M, N).


%% split(+L,?P,?D)

pari(X) :- 0 is X mod 2.

split([], [], []).
split([X|Rest], [X|P], D) :- pari(X), split(Rest, P, D), !.
split([X|Rest], P, [X|D]) :- split(Rest, P, D).


%% prefisso(Pre,L)

prefisso([], _) :- !.
prefisso([_|Rest],[_|L]) :- prefisso(Rest,L).


%% suffisso(Suf,L)

suffisso(Suf,L) :- append(_, Suf, L).


%% sublist(S,L)

sublist(S, L) :- append(_, S, W), append(W, _, L).


%% subset(+Sub,?Set)

subset([], _) :- !.
subset([X|Rest], Set) :- member(X, Set), subset(Rest, Set).


%% rev(+X,?Y)

rev([], []) :- !.
rev([X|Rest], Y) :- append(Tser, [X], Y), rev(Rest, Tser), !.


%% del_first(+X,+L,?Resto)  (fails if X is not a member)

del_first(_,[],_) :- fail. %% not really necessary
del_first(X,[X|Rest],Rest) :- !. 
del_first(X,[Y|Rest],Clean) :- del_first(X,Rest,CleanNoY), append([Y],CleanNoY, Clean). %% X different from Y, because of the previous cut


%% del(+X,+L,?Resto)

del(_,[],[]) :- !. %% not really necessary
del(X,L,L) :- \+ member(X, L), !. 
del(X,Y,Clean) :- del_first(X,Y,PartialClean), del(X,PartialClean,Clean).


%% subst(+X,+Y,+L,-Nuova)

subst(X,_,L,L) :- \+ member(X, L), !. 
subst(X,Y,[X|RestA],[Y|RestB]) :- subst(X,Y,RestA,RestB), !.
subst(X,Y,[Z|RestA],[Z|RestB]) :- subst(X,Y,RestA,RestB).


%% mkset(+L,-Set)

mkset([],[]).
mkset([X|Rest],[X|Set]):- \+ member(X,Rest), !,mkset(Rest,Set).
mkset([_|Rest],Set) :- mkset(Rest,Set).


%% union(+A,+B,-Union)

union(A,B,Union) :- append(A, B, Append), mkset(Append, Union).


%% occurs in(?X,+Y)

occurs_in(_, []) :- !, fail. 
occurs_in(X, X). 
occurs_in(X, [Y|Rest]) :- occurs_in(X, Y); occurs_in(X, Rest).


%% flat(+X,?Y)

flat([], []) :- !.
flat(X, [X]) :- \+ is_list(X), !.
flat([X|Rest], Flat) :- flat(X, FlatOne), flat(Rest, FlatTwo), append(FlatOne, FlatTwo, Flat).




