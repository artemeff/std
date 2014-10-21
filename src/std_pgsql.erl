-module(std_pgsql).
-export([ params_to_hstore/1
        , result_to_proplist/1
        , result_to_agg/1
        ]).

%%
%% Types
%%

-type column_type() :: atom().
-type column_name() :: binary().
-type column() :: {column, column_name(), column_type(), integer(), integer(), integer()}.
-type columns() :: [column()].

%%
%% API
%%

-spec params_to_hstore(map() | list())
   -> string().
params_to_hstore(Params) when is_map(Params) ->
    params_to_hstore(maps:to_list(Params));
params_to_hstore(Params) when is_list(Params) ->
    lists:flatten(string:join(lists:map(fun kv_to_hstore_string/1, Params), ", ")).

-spec result_to_proplist({ok, columns(), list()} | {error, term()})
   -> {ok, list()} | {error, term()}.
result_to_proplist({ok, Columns, Data}) ->
    {ok, lists:map(fun(El) ->
        lists:zip(extract_column_names(Columns), tuple_to_list(El))
    end, Data)};
result_to_proplist({error, Msg}) -> {error, Msg}.

-spec result_to_agg({ok, columns(), list()} | {error, term()})
   -> {ok, [{column_name(), [any()]}]} | {error, term()}.
result_to_agg({ok, Columns, Data}) ->
    ColumnsN = lists:zip(extract_column_names(Columns), lists:seq(1, length(Columns))),
    {ok, lists:map(fun({F,N}) ->
        {F, lists:map(fun(R) -> lists:nth(N, tuple_to_list(R)) end, Data)}
    end, ColumnsN)};
result_to_agg({error, Msg}) -> {error, Msg}.

%%
%% Helpers
%%

kv_to_hstore_string({K, V}) ->
    io_lib:format("~s => ~s", [std_cast:to_list(K), std_cast:to_list(V)]).

extract_column_names(Columns) ->
    lists:map(fun({column,Name,_,_,_,_}) ->
        Name
    end, Columns).

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

params_to_hstore_test() ->
    ?assertEqual("a => b, c => 1", params_to_hstore([{a, "b"}, {<<"c">>, 1}])),
    ?assertEqual("a => b, c => 1", params_to_hstore(#{a => "b", <<"c">> => 1})).

result_data() ->
    {ok,
        [ {column, <<"id">>,   int4, 4, -1, 1}
        , {column, <<"text">>, varchar, -1, 259, 1}
        , {column, <<"date">>, timestamp, 8, -1, 1}
        ],
        [ {1, <<"text1">>, {{2013,12,5},{22,9,39.593497}}}
        , {2, <<"text2">>, {{2013,12,6},{12,3,10.123321}}}
        , {3, <<"text3">>, {{2013,12,7},{18,9,23.656533}}}
        ]}.

result_to_proplist_test() ->
    {ok, Proplist} = result_to_proplist(result_data()),
    Record2 = lists:nth(2, Proplist),
    ?assertEqual(2, proplists:get_value(<<"id">>, Record2)),
    ?assertEqual(<<"text2">>, proplists:get_value(<<"text">>, Record2)),
    ?assertEqual({{2013,12,6},{12,3,10.123321}}, proplists:get_value(<<"date">>, Record2)).

result_to_agg_test() ->
    {ok, Aggregated} = result_to_agg(result_data()),
    ?assertEqual([1, 2, 3], proplists:get_value(<<"id">>, Aggregated)),
    ?assertEqual([<<"text1">>,<<"text2">>,<<"text3">>], proplists:get_value(<<"text">>, Aggregated)).

-endif.
