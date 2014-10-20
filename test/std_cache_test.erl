-module(std_cache_test).
-include_lib("eunit/include/eunit.hrl").

std_cache_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun set_on_one_key/0
    , fun set_with_ttl/0
    , fun set_get_flush_get/0
    ]}.

start() -> std_cache:init().
stop(_) -> std_cache:flush().

%%
%% Tests
%%

set_on_one_key() ->
    ?assertMatch({new, value},
        std_cache:set(set_on_one_key, fun() -> value end)),
    ?assertMatch({exist, value},
        std_cache:set(set_on_one_key, fun() -> value end)).

set_with_ttl() ->
    ?assertMatch({new, value},
        std_cache:set(set_with_ttl, fun() -> value end, 500)),
    ?assertMatch({ok, value},
        std_cache:get(set_with_ttl)),
    receive after 600 ->
        ?assertMatch({error, not_found},
            std_cache:get(set_with_ttl))
    end.

set_get_flush_get() ->
    ?assertMatch({new, value},
        std_cache:set(set_get_flush_get, fun() -> value end)),
    ?assertMatch({ok, value},
        std_cache:get(set_get_flush_get)),
    ?assertMatch(true,
        std_cache:delete(set_get_flush_get)),
    ?assertMatch({error, not_found},
        std_cache:get(set_get_flush_get)).
