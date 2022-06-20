%%%=============================================================================
%%% @copyright 2019, Erlang Solutions Ltd
%%% @doc Unit tests for the ebank backend.
%%% @end
%%%=============================================================================
-module(backend_tests).
-copyright("2019, Erlang Solutions Ltd.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("backend.hrl").

start_link_test() ->
    {ok, Pid} = backend:start_link(),
    ?assert(is_pid(Pid)).

account_test() ->
    ?assertMatch({error, _Msg}, backend:account(100)).

accounts_by_name_test() ->
    ?assertEqual([], backend:accounts_by_name("NoOne")).

list_accounts_test() ->
    ?assertMatch([#account{}, #account{}, #account{}, #account{}], backend:list_accounts()).

pin_valid_test() ->
    ?assertEqual(false, backend:pin_valid(100, "1234")),
    ?assertEqual(false, backend:pin_valid(1, "1111")),
    ?assertEqual(true, backend:pin_valid(1, "1234")).

stop_test() ->
    Result = backend:stop(),
    ?assertEqual(ok, Result).

sequence_test() ->
    {ok, Pid} = backend:start_link(),
    ?assert(is_pid(Pid)),
    ?assertMatch(#account{acc_no = 1,
                          balance = 100,
                          pin = "1234",
                          name = "Henry Nystrom"}, backend:account(1)),
    ?assertMatch({error, _Msg}, backend:account(7)),
    ?assertMatch([#account{acc_no = 1,
                           balance = 100,
                           name = "Henry Nystrom"},
                  #account{acc_no = 4,
                           balance = 5000,
                           name = "Henry Nystrom"}], backend:accounts_by_name("Henry Nystrom")),
    ?assertMatch([#account{}, #account{}, #account{}, #account{}], backend:list_accounts()),
    ?assertEqual(true, backend:pin_valid(1, "1234")),
    ?assertEqual(false, backend:pin_valid(1, "1111")),
    ?assertMatch({error, _Msg}, backend:withdraw(1, "1111", 10)),
    ?assertEqual(ok, backend:withdraw(1, "1234", 10)),
    ?assertMatch({error, _Msg}, backend:withdraw(1, "1234", 100)),
    ?assertEqual(ok, backend:deposit(1, 100)),
    ?assertEqual(ok, backend:transfer(1, 4, 100, "1234")),
    ?assertMatch({error, _Msg}, backend:balance(1, "1111")),
    ?assertEqual(90, backend:balance(1, "1234")),
    ?assertMatch({error, _Msg}, backend:transactions(1, "1111")),
    ?assertMatch([{debit, _, 100},
                  {credit, _, 100},
                  {debit, _, 10},
                  {credit, _, 100}], backend:transactions(1, "1234")),
    ?assertEqual(ok, backend:stop()).
