-module(std_pgsql).
-export([params_to_hstore/1]).

-spec params_to_hstore(map() | list())
   -> string().
params_to_hstore(Params) when is_map(Params) ->
    params_to_hstore(maps:to_list(Params));
params_to_hstore(Params) when is_list(Params) ->
    lists:flatten(string:join(lists:map(fun kv_to_hstore_string/1, Params), ", ")).

%%
%% Helpers
%%

kv_to_hstore_string({K, V}) ->
    io_lib:format("~s => ~s", [std_cast:to_list(K), std_cast:to_list(V)]).

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

params_to_hstore_test() ->
    ?assertEqual("a => b, c => 1", params_to_hstore([{a, "b"}, {<<"c">>, 1}])),
    ?assertEqual("a => b, c => 1", params_to_hstore(#{a => "b", <<"c">> => 1})).

-endif.
