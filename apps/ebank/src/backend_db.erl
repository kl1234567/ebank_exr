-module(backend_db).

-include_lib("backend.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([create_db/0, lookup/2, lookup_by_name/2, new_account/4,
  credit/3, debit/3, is_pin_valid/3, all_accounts/1, close/1]).


-spec(create_db() -> dbref()).
create_db() ->
  ets:new(accounts, [set, protected, named_table, {keypos, #account.acc_no}, {heir, none}, {write_concurrency, false}, {read_concurrency, false},
    {decentralized_counters, false}]).


-spec(lookup(account_number(), dbref()) -> #account{} | {error, instance}).
lookup(AccountNumber, Table) ->
  MatchingObjList = ets:lookup(Table, AccountNumber),
  case length(MatchingObjList) of
    0 -> {error, instance};
    _ -> lists:nth(1, MatchingObjList)
  end.


-spec(lookup_by_name(name(), dbref()) -> [#account{}]).
lookup_by_name(Name, Table) ->
  MatchSpec = ets:fun2ms(fun(Account = #account{name = Namei}) when Namei == Name -> Account end),
  ets:select(Table, MatchSpec).


-spec(new_account(account_number(), pin(), name(), dbref()) -> ok | {error, exists}).
new_account(AccountNumber, Pin, Name, Table) ->
  NewAccount = #account{acc_no = AccountNumber, pin = Pin, name = Name},
  case ets:insert_new(Table, NewAccount) of
    true -> ok;
    false -> {error, exists}
  end.


-spec(credit(account_number(), amount(), dbref()) -> ok | {error, instance}).
credit(AccountNumber, Amount, Table) ->
  CurrAccount = lookup(AccountNumber, Table),
  case CurrAccount of
    {error, instance} -> {error, instance};
    _ -> NewCurrAccount = CurrAccount#account{balance = CurrAccount#account.balance + Amount,
      transactions = [{credit, calendar:local_time(), Amount}] ++ CurrAccount#account.transactions},
      true = ets:insert(Table, NewCurrAccount),
      ok
  end.


-spec(debit(account_number(), amount(), dbref()) -> ok | {error, instance} | {error, balance}).
debit(AccountNumber, Amount, Table) ->
  CurrAccount = lookup(AccountNumber, Table),
  case CurrAccount of
    {error, instance} -> {error, instance};
    _ ->
      case CurrAccount#account.balance - Amount < 0 of
        true -> {error, balance};
        false -> NewCurrAccount = CurrAccount#account{balance = CurrAccount#account.balance - Amount,
          transactions = [{debit, calendar:local_time(), Amount}] ++ CurrAccount#account.transactions},
          true = ets:insert(Table, NewCurrAccount),
          ok
      end
  end.


-spec is_pin_valid(account_number(), pin(), dbref()) -> boolean().
is_pin_valid(AccountNumber, Pin, Table) ->
  CurrAccount = lookup(AccountNumber, Table),
  case CurrAccount of
    {error, instance} -> false;
    _ -> CurrAccount#account.pin =:= Pin
  end.


-spec all_accounts(dbref()) -> [#account{}].
all_accounts(Table) ->
  ets:tab2list(Table).


-spec close(dbref()) -> ok.
close(Table) ->
  true = ets:delete(Table),
  ok.
