-module(std_cache).
-export([ init/0
        , get/1, get/2
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
   -> val() | undefined.
get(Key) ->
    get(Key, undefined).

-spec get(key(), any())
   -> val() | any().
get(Key, Default) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, R}] -> R;
        _          -> Default
    end.

-spec set(key(), val_fun())
   -> val().
set(Key, Fun) ->
    set(Key, Fun, 0).

-spec set(key(), val_fun(), ttl())
   -> val().
set(Key, Fun, TTL) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, R}] -> R;
        _ ->
            Value = Fun(),
            ets:insert(?MODULE, {Key, Value}),
            case TTL of
                0 -> ok;
                TTL ->
                    timer:apply_after(TTL, ?MODULE, delete, [Key])
            end,
            Value
  end.

-spec delete(key())
   -> true.
delete(Key) ->
    ets:delete(?MODULE, Key).

-spec flush()
   -> true.
flush() ->
    ets:delete_all_objects(?MODULE).
