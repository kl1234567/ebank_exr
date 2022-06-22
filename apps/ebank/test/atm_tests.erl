%%%=============================================================================
%%% @copyright 2019, Erlang Solutions Ltd
%%% @doc Unit tests for the ebank atm.
%%% @end
%%%=============================================================================
-module(atm_tests).
-copyright("2019, Erlang Solutions Ltd.").

-include_lib("eunit/include/eunit.hrl").

-define(ATM, atm1).

start_test() ->
    {ok, Pid1} = backend:start_link(),
    {ok, Pid2} = atm:start_link(?ATM),
    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)).

stop_test() ->
    ?assertEqual(ok, atm:stop(?ATM)),
    ?assertEqual(ok, backend:stop()).
