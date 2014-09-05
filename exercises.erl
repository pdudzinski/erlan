-module(exercises).
-export([len/1, lenr/1, duplicate/2, duplicater/2, sum/1, sumr/1,
         reverse/1, reverser/1, sublist/2, sublistr/2, zip/2, zipr/2,
         old/1, oldr/1, maxc/1, minc/1, fold/3, fold2/3, find_way/2]).
 
%EX.1 write len(list) function a) regular recursion b) tail recursion

len([]) -> 0;
len([_|T]) -> 1 + len(T).

lenr(L) -> lenr(L, 0).
lenr([], Acc) -> Acc;
lenr([_|T], Acc) -> lenr(T, Acc+1).

%EX.2 write duplicate(term, times) function that will return [term, ..., term x times] as above

duplicate(_, 0) -> [];
duplicate(Term, N) -> [Term|duplicate(Term, N-1)].

duplicater(Term, N) -> duplicater(Term, N, []).
duplicater(_, 0, Acc) -> Acc;
duplicater(Term, N, Acc) -> duplicater(Term, N-1, [Term|Acc]).

%EX.3 write sum(list) as above

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

sumr(L) -> sumr(L, 0).
sumr([], Acc) -> Acc;
sumr([H|T], Acc) -> sumr(T, Acc+H).

%EX.4 write reverse(list) as above

reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H]. 

reverser(L) -> reverser(L, []).
reverser([], Acc) -> Acc;
reverser([H|T], Acc) -> reverser(T, [H|Acc]).

%EX.5 write sublist(list, X) to return all elemets of list that are > X as above

sublist([], _) -> [];
sublist([H|T], X) when H > X -> [H|sublist(T, X)];
sublist([H|T], X) when H =< X -> sublist(T, X).

sublistr(L, X) -> sublistr(L, X, []).
sublistr([], _, Acc) -> Acc;
sublistr([H|T], X, Acc) when H > X -> sublistr(T, X, Acc ++ [H]);
sublistr([_|T], X, Acc) -> sublistr(T, X, Acc).

%EX.6 write zip(L1, L2) that will return touples ({L1.1, L2.1} ... {L1.N, L2.N}) as above

zip([],[]) -> [];
zip([H1|T1], [H2|T2]) -> [{H1, H2}|zip(T1, T2)].

zipr(L1, L2) -> zipr(L1, L2, []).
zipr([], [], Acc) -> lists:reverse(Acc);
zipr([H1|T1], [H2|T2], Acc) -> zipr(T1, T2, [{H1, H2}|Acc]).

%EX.7 write old(L) only keep men older than 60 as above

old([]) -> [];
old([H|T]) when H > 60 -> [H|old(T)];
old([_|T]) -> old(T).  

oldr(L) -> oldr(L, []).
oldr([], Acc) -> lists:reverse(Acc);
oldr([H|T], Acc) when H > 60 -> oldr(T, [H|Acc]);
oldr([_|T], Acc) -> oldr(T, Acc).

%%EX.8 find the maximum and minimum of a list and an abstraction

maxc([H|T]) -> maxc(T, H).
maxc([], Acc) -> Acc;
maxc([H|T], Acc) when H > Acc -> maxc(T, H);
maxc([_|T], Acc) -> maxc(T, Acc).

minc([H|T]) -> minc(T, H).
minc([], Acc) -> Acc;
minc([H|T], Acc) when H < Acc -> minc(T, H);
minc([_|T], Acc) -> minc(T, Acc).

%%EX.9 write fold function like map

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, [F(H)|Start], T).

fold2(_, Start, []) -> Start;
fold2(F, Start, [H|T]) -> fold2(F, F(H,Start), T).

%%EX.10 write function that finds fastes way where you have only 2 options to consider at a time
%It should return 25
%Road = [{10,20}, {5,2}, {13,33}]

find_way(L, Start) -> fold2(fun({A, B}, Start) when A > B -> B+Start; ({A, _}, Start) -> A+Start end, Start, L).








