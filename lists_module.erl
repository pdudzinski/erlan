-module(lists_module).


%% ALL and ANY

F = fun(X) when X > 4 -> true; (_) -> false end.
L = [1,2,3,4,5,6,7,8,9].

lists:all(F, L).
lists:any(F, L).
