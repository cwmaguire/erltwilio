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
-export([from_form/2]).
-export([handle/2]).
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([terminate/3]).

-record(state, {}).

to_html(Req, State) ->
    Rows = [sms_to_list(SMS_) || SMS_ <- erltwilio:get_sms()],
    {["SMS Messages:<br>", html_table(Rows)], Req, State}.

from_form(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    erltwilio:add_sms(Body),
    {true, Req2, State}.

handle(Req, State) ->
    {ok, Req, State}.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%% for POST/PUT, points to handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, from_form}],
     Req, State}.

%% For GET requests, points to handler
content_types_provided(Req, State) ->
    {[{{<<"text">>,<<"html">>,'*'}, to_html}], Req, State}. %% This is the default

terminate(_, _, _) ->
    ok.

html_table(Rows) ->
    ["<table>", [html_row(Row) || Row <- Rows], "</table>"].

html_row(Cols) ->
    ["<tr>", [["<td>", Col, "</td>"] || Col <- Cols], "</tr>"].

sms_to_list({Timestamp, SMS}) ->
    Time = timestamp_to_list(Timestamp),
    Body = proplists:get_value(<<"Body">>, SMS, "undefined"),
    From = proplists:get_value(<<"From">>, SMS, "undefined"),
    [Time, From, Body].

timestamp_to_list(Timestamp) ->
    {{Y,M,D},{H,Mi,S}} = calendar:now_to_datetime(Timestamp),
    io_lib:format("~p/~p/~p ~p:~p:~p", [Y,M,D,H,Mi,S]).
