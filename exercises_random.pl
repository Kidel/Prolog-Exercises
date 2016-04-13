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