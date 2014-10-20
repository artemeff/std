-module(std).
-export([timestamp/0]).

timestamp() ->
    {Mega, Seconds, _} = now(),
    Mega * 1000000 + Seconds.

%%
%% Test
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
