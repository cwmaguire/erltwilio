%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% Route drawing commands to and from a web page to an Erlang process
-module(erltwilio_cowboy).
-behaviour(cowboy_http_handler).

-export([to_html/2]).
-export([from_json/2]).
-export([handle/2]).
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/0]).
-export([content_types_accepted/0]).
-export([content_types_provided/0]).
-export([terminate/3]).

-record(state, {}).

to_html(Req, State) ->
    {"Hello from Erlang", Req, State}.

from_json(Req, State) ->
    io:format("Got json POST~n"),
    {true, Req, State}.

handle(Req, State) ->
    {ok, Req, State}.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

allowed_methods() ->
    [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"OPTIONS">>].

%% for POST/PUT, points to handler
content_types_accepted() ->
    [{<<"application">>, <<"json">>, '*'}, from_json].

%% For GET requests, points to handler
content_types_provided() ->
    [{{<<"text">>,<<"html">>,'*'}, to_html}]. %% This is the default

terminate(_, _, _) ->
    ok.
