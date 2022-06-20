%%%=============================================================================
%%% @copyright 2019, Erlang Solutions Ltd
%%% @doc Unit tests for the ebank backend_db.
%%% @end
%%%=============================================================================
-module(backend_db_tests).
-copyright("2019, Erlang Solutions Ltd.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("backend.hrl").

create_db_test() ->
    Ref = backend_db:create_db(),
    ?assertEqual(accounts, Ref),
    ?assertEqual(set, ets:info(accounts, type)).

lookup_test() ->
    ?assertEqual({error, instance}, backend_db:lookup(1, accounts)).

new_account_test() ->
    Result1 = backend_db:new_account(1, "1234", "Donald Duck", accounts),
    ?assertEqual(ok, Result1),
    Result2 = backend_db:new_account(1, "1234", "Donald Duck", accounts),
    ?assertEqual({error, exists}, Result2).

lookup_by_name_test() ->
    ?assertEqual([], backend_db:lookup_by_name("NoName", accounts)).

close_db_test() ->
    ?assertEqual(ok, backend_db:close(accounts)).

sequence_test() ->
    DB = backend_db:create_db(),
    ?assertEqual(accounts, DB),
    ?assertEqual(ok, backend_db:new_account(1, "1234", "Donald Duck", DB)),
    ?assertEqual(#account{acc_no = 1,
                          balance = 0,
                          pin = "1234",
                          name = "Donald Duck",
                          transactions = []}, backend_db:lookup(1, DB)),
    ?assertEqual(ok, backend_db:credit(1, 100, DB)),
    #account{acc_no = 1,
             balance = 100,
             pin = "1234",
             name = "Donald Duck",
             transactions = [{credit, Timestamp1, 100}]} = backend_db:lookup(1, DB),
    ?assertMatch({{_, _, _}, {_, _, _}}, Timestamp1),
    ?assertEqual(ok, backend_db:credit(1, 100, DB)),
    #account{acc_no = 1,
             balance = 200,
             pin = "1234",
             name = "Donald Duck",
             transactions = [{credit, Timestamp2, 100},
                             {credit, Timestamp1, 100}]} =  backend_db:lookup(1, DB),
    ?assertMatch({{_, _, _}, {_, _, _}}, Timestamp2),
    ?assertEqual(ok, backend_db:debit(1, 50, DB)),
    #account{acc_no = 1,
             balance = 150,
             pin = "1234",
             name = "Donald Duck",
             transactions = [{debit, Timestamp3, 50},
                             {credit, Timestamp2, 100},
                             {credit, Timestamp1, 100}]} = backend_db:lookup(1, DB),
    ?assertMatch({{_, _, _}, {_, _, _}}, Timestamp3),
    ?assertEqual({error, balance},  backend_db:debit(1, 200, DB)),
    ?assertEqual(true, backend_db:is_pin_valid(1, "1234", DB)),
    ?assertEqual(false, backend_db:is_pin_valid(1, "1111", DB)),
    ?assertEqual(ok, backend_db:close(DB)).


my_test() ->
  AccountNumber = 1,
  Pin = "123",
  Name = "lala",
  T = backend_db:create_db(),
  ok = backend_db:new_account(AccountNumber, Pin, Name, T),
  [#account{acc_no = AccountNumber,
    pin = Pin,
    name = Name}] = backend_db:lookup_by_name(Name, T),
  [] = backend_db:lookup_by_name("la", T),
  #account{} = backend_db:lookup(AccountNumber, T),
  ok = io:format("before: ~p~n", [backend_db:lookup(AccountNumber, T)]),
  ok = backend_db:credit(AccountNumber, 1000, T),
  ok = io:format("after 1: ~p~n", [backend_db:lookup(AccountNumber, T)]),
  ok = backend_db:debit(AccountNumber, 1000, T),
  ok = io:format("after 2: ~p~n", [backend_db:lookup(AccountNumber, T)]),
  {error, balance} = backend_db:debit(AccountNumber, 1000, T),
  ok = io:format("after 3: ~p~n", [backend_db:lookup(AccountNumber, T)]),
  true = backend_db:is_pin_valid(AccountNumber, Pin, T),
  false = backend_db:is_pin_valid(2, Pin, T),
  false = backend_db:is_pin_valid(1, "999", T),
  ok = io:format("all accounts: ~p~n", [backend_db:all_accounts(T)]),
  ok = backend_db:close(T).

