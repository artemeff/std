-module(std).
-export([ timestamp/0
        , priv_dir/1
        ]).

-spec timestamp()
   -> non_neg_integer().
timestamp() ->
    {Mega, Seconds, _} = now(),
    Mega * 1000000 + Seconds.

-spec priv_dir(atom())
   -> string().
priv_dir(App) ->
   Ebin = filename:dirname(code:which(App)),
   filename:join(filename:dirname(Ebin), "priv").

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
