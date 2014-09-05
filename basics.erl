-module(learn).
-export([add/2, head/1, phead/1, old_enough/1, ifelse/1,
         help_me/1, insert/2, beach/1, area/1]).

%simple pattern matching

phead(List) -> 
    [H|_] = List,
    H.

head([H|T]) ->
    H.

text_info(Text) ->
    io:format(Text).

add(A, B) -> 
    text_info("Teraz dodam A i B: "),
    text_info("~n"),
    A + B.
    
area({rectangle, Width, Ht}) ->                 %handling different patterns
    if Width =:= Ht -> area({square, Ht});      %using functions as return
       true -> Width * Ht
    end;
area({square, X}) -> X * X;
area({circle, R}) -> 3.14159 * R * R.

%-------------------------

%guards - conditions in function head

old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when X >= 16, X =< 104 -> true;      %"," acts like andalso
right_age(_) -> false.

wrong_age(X) when X < 16; X > 104 -> true;        %";" acts like orelse
wrong_age(_) -> false.

%-------------------------

%guard patterns - well, "ifs"

ifelse(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does                          %this is Erlang's if's 'else!'
    end.


help_me(Animal) ->                                 %it's better to put below's if in function head as guard
    Talk = if Animal == cat  -> "meow";             %result of if is bounded to Talk variable
              Animal == beef -> "mooo";
              Animal == dog  -> "bark";
              Animal == tree -> "bark";
              true -> "blablabla"
           end,
    {Animal, " says " ++ Talk ++ "!"}.
    
%-------------------------

%case syntax (In Case ... of)

insert(X,[]) -> [X];                               %pattern matching on empty list
insert(X,Set) ->                                   %pattern matching on some variable
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 -> 'favorable';
        {kelvin, N} when N >= 293, N =< 318 -> 'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 -> 'favorable in the US';
        _ -> 'Wrong param.'                        %this is "else" for case
    end.

%above can be written as guards like that:
%beachf({celsius, N}) when N >= 20, N =< 45 -> 'favorable';
%...
%beachf(_) -> 'Wrong param'.

%-------------------------









