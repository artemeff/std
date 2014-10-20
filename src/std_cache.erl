-module(std_cache).
-export([ init/0
        , get/1
        , set/2, set/3
        , delete/1
        , flush/0
        ]).

%%
%% Types
%%

-type key()     :: term().
-type val()     :: term().
-type ttl()     :: non_neg_integer().
-type val_fun() :: fun(() -> term()).

%%
%% API
%%

-spec init()
   -> ok.
init() ->
    ?MODULE = ets:new(?MODULE,
        [ named_table, public
        , {read_concurrency, true}
        , {write_concurrency, true}
        ]), ok.

-spec get(key())
   -> {ok, val()} | {error, not_found}.
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, R}] -> {ok, R};
        _          -> {error, not_found}
    end.

-spec set(key(), val_fun())
   -> {exist, val()} | {new, val()}.
set(Key, Fun) ->
    set(Key, Fun, 0).

-spec set(key(), val_fun(), ttl())
   -> {exist, val()} | {new, val()}.
set(Key, Fun, TTL) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, R}] -> {exist, R};
        _ ->
            Value = Fun(),
            ets:insert(?MODULE, {Key, Value}),
            case TTL of
                0 -> ok;
                TTL ->
                    timer:apply_after(TTL, ?MODULE, delete, [Key])
            end,
            {new, Value}
  end.

-spec delete(key())
   -> true.
delete(Key) ->
    ets:delete(?MODULE, Key).

-spec flush()
   -> true.
flush() ->
    ets:delete_all_objects(?MODULE).
