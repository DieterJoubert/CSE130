%%%%%%%%%%%%% FALL 2013 %%%%%%%%%%%%%%%%

link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
link(new_york, chicago).
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).

path_2(X,Y) :- link(X,Z), link(Z,Y).

path_3(X,Y) :- path_2(X,Z), link(Z,Y).

path_N(A,B,N) :- N=1, link(A,B).
path_N(A,B,N) :- link(C,B), M is N-1, N>1, path_N(A,C,M). 

path(A,B) :- path_helper( A, B, [A]).
path_helper(A,B,Seen) :- link(A,B), not(member(B,Seen)).
path_helper(A,B,Seen) :- link(A,C), not(member(C,Seen)), path_helper(C, B, [C|Seen]).

%%%%%%%%%%%%% SPRING 2013 %%%%%%%%%%%%%%%%

zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

part([],P,[],[]).
part([H|T], P, [H|L1], L2) :- H <= P, part(T,P,L1,L2).
part([H|T], P, L1, [H|L2]) :- H > P, part(T,P,L1,L2).

qsort([], R).
qsort([H|T], R) :- part(T,H,L1,L2), qsort(L1,R1), qsort(L2,R2), append(R1, [H|R2], R).

%%%%%%%%%%%%% WINTER 2013 %%%%%%%%%%%%%%%%

sorted([]).
sorted([_]).
sorted([A,B|T]) :- (A=<B), sorted([B|T]).

sort2(L1,L2) :- permutation(L1,L2), !, sorted(L2).

split([],[],[]).
split([X],[X],[]).
split( [X|T], [X|T2], L3) :- split(T, L3 , T2).

merge([],L,L).
merge(L,[],L).
merge( [H1|T1], [H2|T2], [H1|R] ) :-  H1<H2, merge(T1, [H2|T2], R).
merge( [H1|T1], [H2|T2], [H2|R] ) :-  H1>=H2, merge( [H1|T1], T2, R).

merge_sort( [], []).
merge_sort( [X], [X]).
merge_sort(L,S).

%%%%%%%%%%%%% WINTER 2012 %%%%%%%%%%%%%%%%

remove_all(X,[],[]).
remove_all(X,[H1|T1], L) :- H1=X, remove_all(X,T1,L).
remove_all(X,[H1|T1],[H1|T2]) :- not(H1=X), remove_all(X,T1,T2).

remove_first( _, [], []).
remove_first(X, [H|T], L2) :- H=X, T=L2.
remove_first(X, [H|T], [H|L2]) :- not(H=X), remove_first(X, T, L2).

prefix([],_).
prefix([H|T1],[H|T2]) :- prefix(T1,T2).

segment(A,B) :- prefix(A,B).
segment(L,[_|T]) :- segment(L,T).

%%%%%%%%%%%%% WINTER 2011 %%%%%%%%%%%%%%%%







