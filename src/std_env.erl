-module(std_env).
-export([ set/1
        , get/0
        ]).

set(Value) ->
    Env = case Value of
        production -> "production";
        test       -> "test";
        _          -> "development"
    end, os:putenv("ERL_ENV", Env).

get() ->
    std_cache:set(erl_env, fun() ->
        case os:getenv("ERL_ENV") of
            "production"  -> production;
            "test"        -> test;
            _             -> development
        end
    end).
