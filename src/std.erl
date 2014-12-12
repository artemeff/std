-module(std).
-export([ timestamp/0, timestamp/1
        , priv_dir/1
        , seconds_in/1
        , generate_random_string/1
        , chunk_list/2, chunk_list/3
        ]).

-spec timestamp()
   -> non_neg_integer().
timestamp() ->
    timestamp(now()).

-spec timestamp(erlang:timestamp())
   -> non_neg_integer().
timestamp({Mega, Seconds, _}) ->
    Mega * 1000000 + Seconds.

-spec seconds_in(atom())
   -> non_neg_integer().
seconds_in(minute) -> 60;
seconds_in(hour)   -> seconds_in(minute) * 60;
seconds_in(day)    -> seconds_in(hour) * 24;
seconds_in(week)   -> seconds_in(day) * 7;
seconds_in(_)      -> 1.

-spec priv_dir(atom())
   -> string().
priv_dir(App) ->
   Ebin = filename:dirname(code:which(App)),
   filename:join(filename:dirname(Ebin), "priv").

-spec generate_random_string(non_neg_integer())
   -> binary().
generate_random_string(Size) when Size > 0 ->
    {X,Y,Z} = now(), random:seed(X,Y,Z),
    Initial = random:uniform(62) - 1,
    generate_random_string(<<Initial>>, Size - 1);
generate_random_string(_) -> <<>>.
generate_random_string(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
    << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
generate_random_string(Bin, Rem) ->
    Next = random:uniform(62) - 1,
    generate_random_string(<<Bin/binary, Next>>, Rem - 1).

-spec chunk_list(list(), non_neg_integer())
   -> [[any(), ...]].
chunk_list(List, Len) ->
    chunk_list(List, Len, undefined).
-spec chunk_list(list(), non_neg_integer(), any())
   -> [[any(), ...]].
chunk_list(List, Len, Placeholder) ->
    LeaderLength = case length(List) rem Len of
        0 -> 0;
        N -> Len - N
    end,
    Leader = lists:duplicate(LeaderLength, Placeholder),
    chunk_list(Leader ++ lists:reverse(List), [], 0, Len).
chunk_list([], Acc, _, _) -> Acc;
chunk_list([H|T], Acc, Pos, Max) when Pos == Max ->
    chunk_list(T, [[H] | Acc], 1, Max);
chunk_list([H|T], [HAcc | TAcc], Pos, Max) ->
    chunk_list(T, [[H | HAcc] | TAcc], Pos + 1, Max);
chunk_list([H|T], [], Pos, Max) ->
    chunk_list(T, [[H]], Pos + 1, Max).

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

chunk_list_test() ->
    ?assertEqual([[1, 2, 3], [4, 5, undefined]],
        chunk_list([1, 2, 3, 4, 5], 3)),
    ?assertEqual([[1, 2, 3], [4, 5, zero]],
        chunk_list([1, 2, 3, 4, 5], 3, zero)).

-endif.
