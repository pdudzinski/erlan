-module(p99).
-export([last/1, penultimate/1, element_at/2, lengthm/1,
         reverse/1, is_palindrome/1, flatten/1, duplicates/1,
         pack/1]).

%P01. Find last element of the list
%L = [1,2,3,4,5,6]

last([]) -> nil;
last(L) -> [H|_] = lists:reverse(L), H. 

%P02. Find penultimate element of the list.

penultimate([]) -> nil;
penultimate([H1, _ | []]) -> H1;
penultimate([_|T]) -> penultimate(T).

%P03. Find the K’th element of a list.

element_at([], _) -> nil;
element_at([H|_], 0) -> H;
element_at([_|T], K) -> element_at(T, K-1).

%P04. Find the number of elements of a list.
lengthm([]) -> 0;
lengthm([_|T]) -> 1 + lengthm(T).

%P05. Reverse a list.
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

%P06. Find out whether a list is a palindrome.
is_palindrome([]) -> true;
is_palindrome(L) -> 
    P = reverse(L),
    L =:= P.

%P07. Flatten a nested list structure.
flatten([]) -> [];
flatten([[H|T1]|T2]) -> [H|flatten(T1)] ++ T2;
flatten([H|T]) -> [H|flatten(T)].

%P08. Eliminate consecutive duplicates of list elements.
duplicates([]) -> [];
duplicates([H|[H|T]]) -> duplicates([H|T]);
duplicates([H|[H1|T]])  -> [H|duplicates([H1|T])];
duplicates([H|[]]) -> [H].

%P09 Pack consecutive duplicates of list elements into sublists.
pack([])-> [];
pack([H|[]])-> [H];
pack([[H|T1]|[H|T2]])-> pack([[H|[H|T1]]|T2]);
pack([[H1|T1]|[H2|[]]])-> [[H1|T1],[H2]];
pack([[H1|T1]|[H2|T2]])-> [[H1|T1]|pack([H2|T2])];
pack([H|[H|T]])-> pack([[H,H]|T]);
pack([H1|[H2|T]])-> [H1]|pack([H2|T])].








