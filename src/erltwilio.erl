-module(erltwilio).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([add_sms/1]).
-export([get_sms/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {sms = [] :: list()}).

-define(MAX_SMS, 30).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_sms(SMS) ->
    gen_server:cast(?MODULE, {sms, SMS}).

get_sms() ->
    gen_server:call(?MODULE, get_sms).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(get_sms, _From, #state{sms = SMS} = State) ->
	{reply, SMS, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({sms, NewSMS}, #state{sms = SMS} = State) ->
    NewestMessages = lists:sublist(SMS, ?MAX_SMS - 1),
    NewList = [{os:timestamp(), NewSMS} | NewestMessages],
	{noreply, State#state{sms = NewList}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
