%% mergesort(+L, -M)

mergesort([],[]) :- !. 
mergesort([A],[A]) :- !. 
mergesort(L,S) :-  
			halve(L,L1,L2),
			mergesort(L1,S1),
			mergesort(L2,S2),
			merge(S1,S2,S), !.

halve(List,A,B) :- halve(List,List,A,B), !.
halve(B,[],[],B).
halve(B,[_],[],B).
halve([H|T],[_,_|T2],[H|A],B) :-halve(T,T2,A,B). 

merge(A,[],A).
merge([],B,B).
merge([A|Ra],[B|Rb],[A|M]) :-  A =< B, merge(Ra,[B|Rb],M).
merge([A|Ra],[B|Rb],[B|M]) :-  A > B,  merge([A|Ra],Rb,M).


%% delete_leaf(?Leaf, +Tree, ?NewTree)

delete_leaf(X,t(X,empty,empty),empty).
delete_leaf(X,t(Y,Left1,Right),t(Y,Left2,Right)) :- delete_leaf(X,Left1,Left2).
delete_leaf(X,t(Y,Left,Right1),t(Y,Left,Right2)) :- delete_leaf(X,Right1,Right2).


%% replace_node(+Node, ?Replace, +Tree, ?NewTree)

replace_node(A, B, t(A,C,D), t(B,C,D)).
replace_node(A, B, t(E, C1, D), t(E, C2, D)) :- replace_node(A, B, C1, C2).
replace_node(A, B, t(E, C, D1), t(E, C, D2)) :- replace_node(A, B, D1, D2).