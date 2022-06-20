%%%-------------------------------------------------------------------
%% @doc ebank public API
%% @end
%%%-------------------------------------------------------------------

-module(ebank_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ebank_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
