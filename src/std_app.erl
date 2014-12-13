-module(std_app).
-behaviour(application).
-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    std_cache:init(),
    std_sup:start_link().

stop(_State) ->
    std_cache:flush(),
    ok.
