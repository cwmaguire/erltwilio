-module(erltwilio_app).
-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).
-define(DEFAULT_PORT, 8080).
-define(HEROKU_PORT_ENV_VAR, "PORT").

start() ->
    application:ensure_all_started(erltwilio).

start(_Type, _Args) ->
    Paths = [{"/", erltwilio_cowboy, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, erltwilio, "static"}}],
    Routes = [{?ANY_HOST, Paths}],
    Port = port(),
    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_http(erltwilio_http_listener, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    erltwilio_sup:start_link().

stop(_State) ->
	ok.

port() ->
    case os:getenv(?HEROKU_PORT_ENV_VAR) of
        false ->
            ?DEFAULT_PORT;
        EnvPort ->
            list_to_integer(EnvPort)
    end.
