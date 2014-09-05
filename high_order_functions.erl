-module(high_order_functions).
-export([map/2, even/1, evenr/1, event/1, reverse/1]).
 
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].
 
decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

%now we use a function as an argument for another function
%different way to recurse

incr(X) -> X + 1.
decr(X) -> X - 1.

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
 
%to invoke map use (keyword fun, name of module:function/airyty, <params>):
%high_order_functions:map(fun high_order_functions:incr/1, L).
%high_order_functions:map(fun high_order_functions:decr/1, L).

%%ANONYMOUS FUNCTIONS (FUNS) - functions declared on the fly
%letting you declare a special kind of function inline, without naming them

%fun(Args1) -> Expression1, Exp2, ..., ExpN;
%   (Args2) -> Expression1, Exp2, ..., ExpN;
%   (Args3) -> Expression1, Exp2, ..., ExpN
%end

%example above with FUNS
%high_order_functions:map(fun(X) -> X + 1 end, L).

%%CLOJURES

%PrepareAlarm = fun(Room) -> io:format("Alarm set in ~s.~n",[Room]),
%                   fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
%               end.

even(List) -> [X || X <- List, X rem 2 == 0].

evenr([]) -> [];
evenr([H|T]) when H rem 2 =:= 0 -> [H|even(T)];
evenr([H|T]) when H rem 2 =/= 0 -> evenr(T).

event(L) -> event(L, []).
event([], Acc) -> lists:reverse(Acc);
event([H|T], Acc) when H rem 2 =:= 0 -> event(T, [H|Acc]);
event([H|T], Acc) when H rem 2 =/= 0 -> event(T, Acc).


%FILTER function - Pred should be fun that checks condidtion to odfiltrowac ;)

filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true  -> filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.

% Numbers = lists:seq(1,10).
% filter(fun(X) -> X rem 2 == 0 end, Numbers).

%FOLD function - any function you can think of that reduces lists to 1 element can be expressed as a fold

maxc([H|T]) -> maxc(T, H).
maxc([], Acc) -> Acc;
maxc([H|T], Acc) when H > Acc -> maxc(T, H);
maxc([_|T], Acc) -> maxc(T, Acc).

minc([H|T]) -> minc(T, H).
minc([], Acc) -> Acc;
minc([H|T], Acc) when H < Acc -> minc(T, H);
minc([_|T], Acc) -> minc(T, Acc).

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

% [H|T] = [1,7,3,5,9,0,2,3].
% fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).     <- max
% fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).     <- min
% fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).           <- sum

% below, reverse, map and filter with fold!

% fold(funkcja, tu_leci_jej_wynik, lista)

reverse(L) -> fold(fun(X,Acc) -> [X|Acc] end, [], L).
 
map2(F,L) -> reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).
 
filter2(Pred, L) ->
    F = fun(X,Acc) ->
    case Pred(X) of
        true  -> [X|Acc];
        false -> Acc
        end
    end,
    reverse(fold(F, [], L)).









