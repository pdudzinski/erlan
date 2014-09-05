-module(tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    T = 3,
    A = 3,
    ?assertEqual(T, A).

to_fail_test() ->
    T = 1,
    A = 2,
    ?assert(T =:= A).
