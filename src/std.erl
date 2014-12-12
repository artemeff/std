-module(std).
-export([ timestamp/0, timestamp/1
        , priv_dir/1
        , generate_random_string/1
        ]).

-spec timestamp()
   -> non_neg_integer().
timestamp() ->
    timestamp(now()).

-spec timestamp(erlang:timestamp())
   -> non_neg_integer().
timestamp({Mega, Seconds, _}) ->
    Mega * 1000000 + Seconds.

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

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
