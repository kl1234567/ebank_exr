%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. יוני 2022 16:59
%%%-------------------------------------------------------------------
-module(backend).
-author("User").

-include_lib("backend.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, account/1, accounts_by_name/1, list_accounts/0, pin_valid/2, withdraw/3, deposit/2, transfer/4, balance/2, transactions/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(backend_state, {table :: ets:table()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).


-spec account(account_number()) -> #account{} | {error, Reason :: term()}.
account(AccountNumber) ->
  gen_server:call(?SERVER, {get_account, AccountNumber}).


-spec accounts_by_name(name()) -> [#account{}].
accounts_by_name(Name) ->
  gen_server:call(?SERVER, {accounts_by_name, Name}).


-spec list_accounts() -> [#account{}].
list_accounts() ->
  gen_server:call(?SERVER, {list_accounts}).


-spec pin_valid(account_number(), pin()) -> boolean().
pin_valid(AccountNumber, Pin) ->
  gen_server:call(?SERVER, {pin_valid, AccountNumber, Pin}).


-spec withdraw(account_number(), pin(), amount()) -> ok | {error, Reason :: term()}.
withdraw(AccountNumber, Pin, Amount) ->
  gen_server:call(?SERVER, {withdraw, AccountNumber, Pin, Amount}).


-spec deposit(account_number(), amount()) -> ok | {error, Reason :: term()}.
deposit(AccountNumber, Amount) ->
  gen_server:call(?SERVER, {deposit, AccountNumber, Amount}).


-spec transfer(account_number(), account_number(), amount(), pin()) -> ok | {error, Reason :: term()}.
transfer(FromAccountNumber, ToAccountNumber, Amount, Pin) ->
  gen_server:call(?SERVER, {transfer, FromAccountNumber, ToAccountNumber, Amount, Pin}).


-spec balance(account_number(), pin()) -> balance() | {error, Reason :: term()}.
balance(AccountNumber, Pin) ->
  gen_server:call(?SERVER, {balance, AccountNumber, Pin}).


-spec transactions(account_number(), pin()) -> transactions() | {error, Reason :: term()}.
transactions(AccountNumber, Pin) ->
  gen_server:call(?SERVER, {transactions, AccountNumber, Pin}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #backend_state{}} | {ok, State :: #backend_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Table = backend_db:create_db(),
  % get accounts from a file and input to table:
  {ok, Accounts} = file:consult(code:priv_dir(ebank) ++ "/accounts.txt"),
  UpdateTable =
    fun({AccountNumber, Balance, Pin, Name}) ->
      ok = backend_db:new_account(AccountNumber, Pin, Name, Table),
      ok = backend_db:credit(AccountNumber, Balance, Table)
    end,
  lists:foreach(UpdateTable, Accounts),
  {ok, #backend_state{table = Table}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #backend_state{}) ->
  {reply, Reply :: term(), NewState :: #backend_state{}} |
  {reply, Reply :: term(), NewState :: #backend_state{}, timeout() | hibernate} |
  {noreply, NewState :: #backend_state{}} |
  {noreply, NewState :: #backend_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #backend_state{}} |
  {stop, Reason :: term(), NewState :: #backend_state{}}).

handle_call({get_account, AccountNumber}, _From, State = #backend_state{table = Table}) ->
  Res = backend_db:lookup(AccountNumber, Table),
  NewRes =
    case Res of
      {error, instance} -> {error, no_account};
      _ -> Res
    end,
  {reply, NewRes, State};

handle_call({accounts_by_name, Name}, _From, State = #backend_state{table = Table}) ->
  Res = backend_db:lookup_by_name(Name, Table),
  {reply, Res, State};

handle_call({list_accounts}, _From, State = #backend_state{table = Table}) ->
  Res = backend_db:all_accounts(Table),
  {reply, Res, State};

handle_call({pin_valid, AccountNumber, Pin}, _From, State = #backend_state{table = Table}) ->
  Res = backend_db:is_pin_valid(AccountNumber, Pin, Table),
  {reply, Res, State};

handle_call({withdraw, AccountNumber, Pin, Amount}, _From, State = #backend_state{table = Table}) ->
  Res = withdraw_action(AccountNumber, Pin, Table, Amount),
  {reply, Res, State};

handle_call({deposit, AccountNumber, Amount}, _From, State = #backend_state{table = Table}) ->
  Res = backend_db:credit(AccountNumber, Amount, Table),
  {reply, Res, State};

handle_call({transfer, FromAccountNumber, ToAccountNumber, Amount, Pin}, _From, State = #backend_state{table = Table}) ->
  Res =
    case backend_db:lookup(ToAccountNumber, Table) of
      {error, instance} -> {error, instance};
      _ -> case withdraw_action(FromAccountNumber, Pin, Table, Amount) of
             ok ->
               backend_db:credit(ToAccountNumber, Amount, Table);
             OtherRes -> OtherRes
           end
    end,
  {reply, Res, State};

handle_call({balance, AccountNumber, Pin}, _From, State = #backend_state{table = Table}) ->
  Res =
    case backend_db:is_pin_valid(AccountNumber, Pin, Table) of
      false -> {error, wrong_pin};
      true -> Account = backend_db:lookup(AccountNumber, Table),
        Account#account.balance
    end,
  {reply, Res, State};

handle_call({transactions, AccountNumber, Pin}, _From, State = #backend_state{table = Table}) ->
  Res =
    case backend_db:is_pin_valid(AccountNumber, Pin, Table) of
      false -> {error, wrong_pin};
      true -> Account = backend_db:lookup(AccountNumber, Table),
        Account#account.transactions
    end,
  {reply, Res, State};

handle_call(_Request, _From, State = #backend_state{}) ->
  {reply, ok, State}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #backend_state{}) ->
  {noreply, NewState :: #backend_state{}} |
  {noreply, NewState :: #backend_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #backend_state{}}).
handle_cast(_Request, State = #backend_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #backend_state{}) ->
  {noreply, NewState :: #backend_state{}} |
  {noreply, NewState :: #backend_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #backend_state{}}).
handle_info(_Info, State = #backend_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #backend_state{}) -> term()).
terminate(_Reason, _State = #backend_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #backend_state{},
    Extra :: term()) ->
  {ok, NewState :: #backend_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #backend_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec withdraw_action(account_number(), pin(), dbref(), amount()) -> ok | {error, Reason :: term()}.
withdraw_action(AccountNumber, Pin, Table, Amount) ->
  case backend_db:is_pin_valid(AccountNumber, Pin, Table) of
    true -> backend_db:debit(AccountNumber, Amount, Table);
    false -> {error, wrong_pin}
  end.
