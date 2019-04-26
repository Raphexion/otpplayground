-module(bar).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% API

-export([start_link/0, stop/0]).
-export([play/2]).

%% Behaviour callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%----------------------------------------------------------
%% API
%%----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

play(Tag, From) ->
    gen_server:call(?MODULE, {play, Tag, From}).

%%----------------------------------------------------------
%% Behaviour callbacks
%%----------------------------------------------------------

init(_) ->
    {ok, #{}}.

handle_call({play, Tag, From}, _From, State) ->
    erlang:send_after(100, self(), {play, Tag, From}),
    {reply, ok, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({play, Tag, From}, State) ->
    gen_server:reply(From, {Tag, bar_was_here}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
