-module(recursion).
-export([for/3, map/2, mapcompr/2, sum/1, list/2, fac/1, facp/1,
         len/1, tail_fac/1, duplicate/2, sum_tail/1, reverse/1,
         reverse_tail/1, zip/2, sublist/2]).

%% FUNS

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

for(Max, Max, F) -> [F(Max)];
for(I, Max, F)  -> [F(I)|for(I+1, Max, F)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

list(Max,Max) -> [Max];
list(I,Max) -> [I|list(I+1, Max)].

%% map with list comprahensions: 
mapcompr(F, L) -> [F(X) || X <- L].

%% TYPICAL RECURSION

% factorial(silnia)
fac(N) when N == 0 -> 1;
fac(N) when N > 0  -> N*fac(N-1).

% factorial with pattern matching and guards
facp(0) -> 1;
facp(N) when N > 0 -> N*fac(N-1).

% clever way to get length of list
len([]) -> 0;
len([_]) -> 1;
len([_|T]) -> 1 + len(T).

%% TAIL RECURSION

% factorial
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).

tail_fac(N) -> tail_fac(N,1).

%length
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,Acc+1).

tail_len(L) -> tail_len(L,0).

%% REGULAR RECURSION VS TAIL

%regular
duplicate(0,_) -> [];
duplicate(N,Term) when N > 0 -> [Term|duplicate(N-1,Term)].

%tail
duplicate_tail(0, _, Acc) -> Acc;
duplicate_tail(N, Term, Acc) -> duplicate_tail(N-1, Term, [Term|Acc]).

duplicate_tail(N, Term) -> duplicate_tail(N, Term, []).

%tail sum
sum_tail([], Acc) -> Acc; 
sum_tail([H|T], Acc) -> sum_tail(T, Acc+H).

sum_tail(List) -> sum_tail(List, 0).

%regular reverse the list
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H]. 

%tail reverse the list
reverse_tail(List) -> reverse_tail(List, []).

reverse_tail([], Acc) -> Acc;
reverse_tail([H|T], Acc) -> reverse_tail(T, [H]++Acc).

%regural sublist
sublist([], _) -> [];
sublist(_, 0) -> [];
sublist([H|T], N) when N > 0 -> [H|sublist(T, N-1)].

%tail sublist

sublist_tail([], _, Acc) -> Acc;
sublist_tail(_, 0, Acc) -> Acc;
sublist_tail([H|T], N, Acc) -> sublist_tail(T, N-1, [H|Acc]).  % [H|Acc] == [H]++Acc
sublist_tail(List, N) -> sublist_tail(List, N, []).

%zip function
%from [1,2,3] and [a,b,c] we should have [{1,a}, {2,b}, {3,c}]

zip([],[]) -> [];
zip([H1|T1], [H2|T2]) -> [{H1, H2}, zip(T1, T2)].

%quicksort - understand!
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) -> 
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot]) ++ [Pivot] ++ lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
