%% genealogico

genitore(tommaso,francesca).
genitore(tommaso,vittorio).
genitore(francesca,linda).
genitore(francesca,leonardo).
genitore(vittorio,bianca).
genitore(vittorio,andrea).
genitore(bianca,tommaso).
genitore(gianroberto,vittorio). 

nonno(X,Y) :- genitore(X,Z), genitore(Z,Y).

bisnonno(X,Y) :- genitore(X,Z), nonno(Z,Y).
fratello(X,Y) :- genitore(Z,X), genitore(Z,Y), X\=Y. 
zio(X,Y) :- genitore(Z,Y), fratello(Z,X).
nipote(X,Y) :- nonno(Y,X); zio(Y,X).
bisnipote(X,Y) :- bisnonno(Y,X). 
discendente(X,Y) :- genitore(Y,X); nonno(Y,X); bisnonno(Y,X); (genitore(Y,Z), discendente(X,Z)).
cugino(X,Y) :- genitore(A,X), genitore(B,Y), fratello(A,B).

%% fact(+X,?Y), vero se Y è il fattoriale di X.

fact(0, 1) :- !.
fact(X, Y) :-  X>0, Z is (X-1), fact(Z, K), Y is (K*X), !.


%% palindroma(X)

palindroma([]).
palindroma([_]).
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


%% hanoi(N)

hanoi(N) :- move(N,'A','B','C'), !.

move(1,Start,Goal,_) :-  
    atomic_list_concat(['Move a disc from',Start,'to',Goal],' ',Out),
    write(Out), 
    nl. 
move(N,Start,Goal,App) :- 
    N>1, 
    M is N-1, 
    move(M,Start,App,Goal), 
    move(1,Start,Goal,_), 
    move(M,App,Goal,Start). 
	

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

del_first(_,[],_) :- fail. %% not necessary
del_first(X,[X|Rest],Rest) :- !. 
del_first(X,[Y|Rest],Clean) :-  %% implicit that X different from Y, because of the previous cut
		del_first(X,Rest,CleanNoY), append([Y],CleanNoY, Clean). 


%% del(+X,+L,?Resto)

del(_,[],[]) :- !. %% not necessary
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

union(A,B,Union) :- append(A,B,Append), mkset(Append,Union).


%% occurs in(?X,+Y)

occurs_in(_, []) :- !, fail. 
occurs_in(X, X). 
occurs_in(X, [Y|Rest]) :- occurs_in(X, Y); occurs_in(X,Rest), X\=Rest.


%% flat(+X,?Y)

flat([], []) :- !.
flat(X, [X]) :- \+ is_list(X), !.
flat([X|Rest],Flat) :- flat(X,FlatOne), flat(Rest,FlatTwo), append(FlatOne,FlatTwo,Flat).


%% cartprod(+A,+B,-Set)

prod_first(_,[],[]).
prod_first(X,[Y|Coda],PartialSet) :- PartialSet = [[X,Y]|Rest], prod_first(X, Coda, Rest).

cartprod([],_,[]) :- !.
cartprod(_,[],[]) :- !.
cartprod([A|Tail],B,Set) :- 
			prod_first(A, B, NewSetA), 
			cartprod(Tail,B,NewSetTail),
			append(NewSetA,NewSetTail,Set), !.
			
cartprod2(A,B,Set) :- findall([X,Y], (member(X,A), member(Y,B)), Set).


%% insert(X,L1,L2)

insert(X,L,[X|L]).
insert(X,[Y|L1],[Y|L2]) :- insert(X,L1,L2).


%% permut(X,Y)

permut([], []).
permut([X|Tail],Permut) :- permut(Tail, Part),  insert(X, Part, Permut).


%% search subset(+IntList,+N,?Set)

search_subset([],0,[]).
search_subset([X|Tail1],N,[X|Tail2]) :- M is N-X, search_subset(Tail1,M,Tail2).
search_subset([_|Tail1],N,Set) :- search_subset(Tail1,N, Set).


%% trees

bin_height(empty,0).
bin_height(t(_,Left,Right),H) :- bin_height(Left,N), bin_height(Right,M), (N>M, H is N+1 ; N=<M, H is M+1).

bin_size(empty,0).
bin_size(t(_,Left,Right),H) :- bin_size(Left,N), bin_size(Right,M), H is N+M+1.

reflect(empty,empty).
reflect(t(X,empty,empty),t(X,empty,empty)).
reflect(t(X,Left,Right),t(X,Right,Left)).

bin_labels(empty,[]).
bin_labels(t(X,empty,empty),[X]).
bin_labels(t(X,Left,Right),[X|Rest]) :- 
		bin_labels(Left,L),
		bin_labels(Right,R),
		append(L,R,Rest).
		
balanced(empty).
balanced(t(_,Left, Right)) :- 
		bin_height(Left, LeftH), 
		bin_height(Right, RightH), 
		X is abs(RightH-LeftH), 
		X=<1, 
		balanced(Left), 
		balanced(Right).
		
branch(t(Leaf,empty, empty),Leaf,[Leaf]).
branch(t(X,Left,_), Leaf, [X|Tail]) :- branch(Left, Leaf, Tail).
branch(t(X,_,Right), Leaf, [X|Tail]) :- branch(Right, Leaf, Tail).

%% Root => Subtrees

op(600,xfx,=>).

height(empty, 0).
height(_ => SubTree,N) :- !, height(SubTree,X), N is X+1.
height([], 0) :- !.
height([X|SubTrees],N) :- !, height(X,This), height(SubTrees,Partial), max(This,Partial,N).
height(_, 1).

size(empty, 0).
size(_ => SubTree,N) :- !, size(SubTree, X), N is X+1.
size([], 0) :- !.
size([X|SubTrees],N) :- !, size(X,This), size(SubTrees,Other), N is This+Other.
size(_, 1).

label(empty, []).
label(Root => SubTree,[Root|Tail]) :- !, label(SubTree, Tail). 
label([], []) :- !.
label([X|SubTrees],Labels) :- !, label(X,LabelX), label(SubTrees,OtherLabels), append(LabelX,OtherLabels,Labels).
label(X, [X]).


%% path 

arc(a,b).
arc(a,e).
arc(b,a).
arc(b,c).
arc(c,c).
arc(c,d).
arc(d,c).
arc(d,b).
arc(e,c).

path(Start, Goal, Path) :- path(Start, Goal, Path, []).

path(Start, Start, [Start], Visited) :- \+ member(Start, Visited).
path(Start, Goal, [Start|Path], Visited) :-
		\+ member(Start, Visited), 
		arc(Start, N), 
		path(N, Goal, Path, [Start|Visited]).


