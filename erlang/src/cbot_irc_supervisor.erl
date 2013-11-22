%%
%% cbot_irc_supervisor
%%
%% This is a supervisor process which manages all processes that belong
%% to a particular IRC bot. This includes:
%%
%%   - cbot_irc_connection, which owns the connection itself
%%     and handles login
%%
%%   - cbot_irc_behaviour, which implements all stateful
%%     higher-level functionality
%%
%% If cbot_irc_conn dies, we've lost the connection so we need to
%% restart cbot_irc_behaviour in order to re-sync the IRC server with
%% our desired bot state. If cbot_irc_behaviour dies, it needs to
%% restart and figure out the server's current state, but the
%% cbot_irc_conn process keeps on going.
%%
%% Thus, we use the 'rest_for_one' restart strategy.
%%
%% Copyright (C) 2007 Micah Dowty <micah@navi.cx>
%%
%%---------------------------------------------------------------------------

-module(cbot_irc_supervisor).
-behaviour(supervisor).

%% Public API
-export([start_link/2, get_child/2]).

%% supervisor callbacks
-export([init/1]).


%%---------------------------------------------------------------------------
%% Public API
%%---------------------------------------------------------------------------

start_link(ConnectInfo, Options) ->
    supervisor:start_link(?MODULE, {ConnectInfo, Options}).

%%
%% Return a child's Pid, or 'undefined'.
%% The child ID should be 'behaviour' or 'connection'.
%%

get_child(Pid, ChildId) ->
    get_child1(supervisor:which_children(Pid), ChildId).

get_child1([], _Id) ->
    undefined;
get_child1([{Id, Pid, _Type, _Modules}|_], Id) ->
    Pid;
get_child1([_|T], Id) ->
    get_child1(T, Id).


%%---------------------------------------------------------------------------
%% Callback interface for supervisor
%%---------------------------------------------------------------------------

init({ConnectInfo, Options}) ->
    {ok, {
       %%
       %% Restart Strategy:
       %%
       %% Note that any time the bot loses its connection, it will
       %% exit. If login fails after LOGIN_TIMEOUT, it will exit.
       %% If connect() fails, after a timeout or not, we will wait
       %% CONNECT_FAIL_DELAY and exit.
       %% 
       %% Those timeouts combined with the supervisor's restart
       %% strategy will define our bot's reconnection behaviour.
       %%
       %% Please note the amount of time that it takes for a
       %% connection to time out, when setting the supervisor
       %% parameters: If CONNECT_FAIL_DELAY is 15000 and
       %% CONNECT_TIMEOUT is 60000, it would take 750 seconds to
       %% restart 10 times even if the bot was doing nothing but
       %% failing to connect and restarting. The parameters must be
       %% set such to avoid this scenario.
       %% 
       { rest_for_one,
	 10,             %% Max restarts (# of restarts)
	 60 * 60         %% Max restarts (in X seconds)
	},

       %% Child processes
       [
	{ connection,
	  {cbot_irc_connection, start_link, [ConnectInfo, Options]},
	  permanent,     %% Restart mode
	  5000,          %% Shutdown Timeout
	  worker,        %% Process type
	  [gen_server]   %% Modules
	 },
	{ behaviour,
	  {cbot_irc_behaviour, start_link, [self(), Options]},
	  permanent,     %% Restart mode
	  5000,          %% Shutdown Timeout
	  worker,        %% Process type
	  [gen_server]   %% Modules
	 }
       ]
      }}.
