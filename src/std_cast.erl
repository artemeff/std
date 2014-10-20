-module(std_cast).
-include("std.hrl").
-export([ to_integer/1
        , to_binary/1
        , to_list/1
        ]).

to_integer(I) when is_integer(I) ->
    I;
to_integer(A) when is_atom(A) ->
    to_integer(?a2l(A));
to_integer(B) when is_binary(B) ->
    to_integer(?b2l(B));
to_integer(L) when is_list(L) ->
    case catch ?l2i(L) of
        {'EXIT', _} -> throw({error, {not_a_valid_integer, L}});
        Int         -> Int
    end.

to_float(F) when is_float(F) ->
    F;
to_float(A) when is_atom(A) ->
    to_float(?a2l(A));
to_float(B) when is_binary(B) ->
    to_float(?b2l(B));
to_float(L) when is_list(L) ->
    case catch ?l2f(L) of
        {'EXIT', _} -> throw({error, {not_a_valid_integer, L}});
        Float       -> Float
    end.

to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    ?l2b(L);
to_binary(A) when is_atom(A) ->
    ?a2b(A);
to_binary(I) when is_integer(I) ->
    ?i2b(I).

to_list(B) when is_binary(B) ->
    ?b2l(B);
to_list(A) when is_atom(A) ->
    ?a2l(A);
to_list(I) when is_integer(I) ->
    ?i2l(I);
to_list(L) when is_list(L) ->
    L.

to_atom(A) when is_atom(A) ->
    A;
to_atom(B) when is_binary(B) ->
    to_atom(?b2l(B));
to_atom(I) when is_integer(I) ->
    to_atom(?i2l(I));
to_atom(L) when is_list(L) ->
    ?l2a(L).

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_test() ->
    ?assertEqual(1, to_integer(1)),
    ?assertEqual(1, to_integer('1')),
    ?assertEqual(1, to_integer("1")),
    ?assertEqual(1, to_integer(<<"1">>)).

to_float_test() ->
    ?assertEqual(1.1, to_float(1.1)),
    ?assertEqual(1.1, to_float('1.1')),
    ?assertEqual(1.1, to_float("1.1")),
    ?assertEqual(1.1, to_float(<<"1.1">>)).

to_binary_test() ->
    ?assertEqual(<<"bin">>, to_binary(<<"bin">>)),
    ?assertEqual(<<"bin">>, to_binary("bin")),
    ?assertEqual(<<"bin">>, to_binary(bin)),
    ?assertEqual(<<"1">>,   to_binary(1)).

to_list_test() ->
    ?assertEqual("list", to_list("list")),
    ?assertEqual("list", to_list(<<"list">>)),
    ?assertEqual("list", to_list(list)),
    ?assertEqual("1",    to_list(1)).

to_atom_test() ->
    ?assertEqual(atom, to_atom(atom)),
    ?assertEqual(atom, to_atom(<<"atom">>)),
    ?assertEqual(atom, to_atom("atom")),
    ?assertEqual('1',  to_atom(1)).

-endif.
