-module(timeoutfun).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% API

-export([start_link/0, stop/0]).
-export([reply_after/2]).

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

reply_after(Msg, Delay) ->
    gen_server:call(?MODULE, {reply_after, Msg, Delay}).

%%----------------------------------------------------------
%% Behaviour callbacks
%%----------------------------------------------------------

-record(state, {cnt = 0}).

init(_) ->
    {ok, #state{}}.

handle_call({reply_after, Msg, Delay}, From, State) ->
    erlang:send_after(Delay, self(), {reply_to_with, From, Msg}),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({reply_to_with, From, Msg}, State=#state{cnt = Cnt}) ->
    gen_server:reply(From, Msg),
    {noreply, State#state{cnt = Cnt + 1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

basic_test_() ->
    {setup,
     fun() -> start_link() end,
     fun(_) -> stop() end,
     {inparallel,
      [?_assertEqual(foobar1, reply_after(foobar1, 300)),
       ?_assertEqual(foobar2, reply_after(foobar2, 200)),
       ?_assertEqual(foobar3, reply_after(foobar3, 100))]}
    }.

advanced_test_() ->
    {setup,
     fun() -> start_link() end,
     fun(_) -> stop() end,
     {inparallel,
      [?_assertEqual(foobar1, reply_after(foobar1, 300)),
       ?_assertEqual(foobar2, reply_after(foobar2, 200)),
       ?_assertEqual(foobar3, reply_after(foobar3, 100)),
       ?_assertEqual(foobar3, reply_after(foobar3, 100)),
       ?_assertEqual(foobar4, reply_after(foobar4, 100)),
       ?_assertEqual(foobar5, reply_after(foobar5, 100)),
       ?_assertEqual(foobar6, reply_after(foobar6, 100)),
       ?_assertEqual(foobar7, reply_after(foobar7, 100)),
       ?_assertEqual(foobar8, reply_after(foobar8, 100)),
       ?_assertEqual(foobar9, reply_after(foobar9, 100)),
       ?_assertEqual(foobar0, reply_after(foobar0, 100))]}}.
