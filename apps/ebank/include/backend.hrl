-export_type([dbref/0, account_number/0, name/0, pin/0, amount/0, balance/0, transactions/0]).

-record(account,
{acc_no :: account_number(),
  balance = 0 :: balance(),
  pin :: pin(),
  name :: name(),
  blocked = false :: boolean(),
  transactions = [] :: transactions()}).

-type dbref() :: ets:table().

-type account_number() :: integer().

-type name() :: string().

-type pin() :: string().

-type amount() :: integer().

-type balance() :: integer().

-type transactions() :: list().