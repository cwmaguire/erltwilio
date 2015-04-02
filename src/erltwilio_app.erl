-module(erltwilio_app).
-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start() ->
    application:ensure_all_started(erltwilio).

start(_Type, _Args) ->
    io:format(standard_error, "Started erltwilio app~n", []),
    Paths = [{"/", erltwilio_cowboy, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, erltwilio, "static"}}],
    Routes = [{?ANY_HOST, Paths}],
    Port = case os:getenv("PORT") of
               false ->
                   io:format(standard_error, "Couldn't find $PORT~n", []),
                   8080;
               EnvPort ->
                   io:format(standard_error, "Found $PORT: ~p~n", [EnvPort]),
                   list_to_integer(EnvPort)
           end,
    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_http(erltwilio_http_listener, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    erltwilio_sup:start_link().

stop(_State) ->
	ok.
