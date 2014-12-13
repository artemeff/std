-module(std_config).
-export([ load/1
        , get/2
        , set/3
        ]).

% Config files loaded (in order)
% 1. <app>.app.src
% 2. config/default.config
% 3. environment specific e.g config/development.config
load(AppName) ->
    DefConf = load_config_file("config/default.config"),
    EnvConf = load_config_file(env_config_file()),
    % we are loading the applications so they don't overwrite
    % the config when application:start/1 is called
    Apps = [AppName | [proplists:get_keys(Config) || Config <- [DefConf, EnvConf]]],
    [application:load(App) || App <- lists:usort(lists:flatten(Apps))],
    % reload the default config from app.src first and then override
    % the environment specific configs
    lists:foreach(fun load_app_config/1, DefConf),
    lists:foreach(fun load_app_config/1, EnvConf).

get(AppName, Key) ->
    application:get_env(AppName, Key).

set(AppName, Key, Value) ->
    application:set_env(AppName, Key, Value).

%%
%% Helpers
%%

load_app_config({App, PList}) ->
    SetEnvFn = fun({Key, Value}) ->
        application:set_env(App, Key, Value)
    end,
    lists:foreach(SetEnvFn, PList).

env_config_file() ->
    Env = std_env:get(),
    io_lib:format("config/~p.config", [Env]).

load_config_file(Path) ->
    case file:consult(Path) of
        {ok, [Terms]} -> Terms;
        _ -> []
    end.
