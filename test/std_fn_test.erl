-module(std_fn_test).
-include_lib("eunit/include/eunit.hrl").

compose_test() ->
    Fn1 = std_fn:compose(
        [ fun(X) -> X + 3 end
        , fun(X) -> X * 4 end
        , fun(X) -> X - 2 end
        ]),
    ?assertEqual(50, Fn1(10)),
    ?assertEqual(62, Fn1(13)).

partial_test() ->
    Fn1 = std_fn:partial(fun lists:map/2,
        [fun erlang:list_to_atom/1]),
    ?assertEqual([test, partial], Fn1(["test", "partial"])),

    Fn2 = std_fn:partial(fun lists:foldl/3,
        [fun(X, Acc) -> X * Acc end, 1]),
    ?assertEqual(100, Fn2([2, 5, 10])).
