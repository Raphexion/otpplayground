-module(foo).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% API

-export([start_link/0, stop/0]).
-export([play/1]).

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

play(Tag) ->
    gen_server:call(?MODULE, {play, Tag}).

%%----------------------------------------------------------
%% Behaviour callbacks
%%----------------------------------------------------------

-record(state, {refs = #{}}).

init(_) ->
    {ok, #state{}}.

handle_call({play, Tag}, From, State) ->
    bar:play({Tag, foo_was_here}, From),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

basic_test_() ->
    {setup,
     fun() ->
	     ?MODULE:start_link(),
	     bar:start_link()

     end,
     fun(_) ->
	     ?MODULE:stop(),
	     bar:stop()
     end,
     {inparallel,
      [?_assertEqual({{paris, foo_was_here}, bar_was_here}, play(paris)),
       ?_assertEqual({{rome, foo_was_here}, bar_was_here}, play(rome)),
       ?_assertEqual({{new_york, foo_was_here}, bar_was_here}, play(new_york))
      ]}
    }.
