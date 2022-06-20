%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. יוני 2022 11:43
%%%-------------------------------------------------------------------
-module(atm).
-author("User").

-behaviour(gen_statem).

%% API
-export([start_link/1, stop/1, event/2, card_inserted/2]).

%% gen_statem callbacks
-export([init/1, format_status/2,
%%  state_name/3,
%%  handle_event/4,
  terminate/3, code_change/4, callback_mode/0,
  state_idle/3, state_get_pin/3]).

-define(SERVER, ?MODULE).

-define(ST_IDLE, state_idle).
-define(ST_GET_PIN, state_get_pin).
-define(ST_SELECTION, state_selection).
-define(ST_WITHDRAW, state_withdraw).

-record(atm_state, {name,
  entered_pin = "",
  account_number = none}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
-spec start_link(atom()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Name) ->
  gen_statem:start_link({global, Name}, ?MODULE, [Name], []).


-spec stop(atom()) -> ok.
stop(Name) ->
  gen_statem:stop({global, Name}).


-spec event(atom(), tuple()) -> ok.
event(Name, Event) ->
  gen_statem:cast({global, Name}, Event).


-spec card_inserted(atom(), backend:account_number()) -> ok.
card_inserted(Name, AccountNumber) ->
  atm:event(Name, {card_inserted, AccountNumber}).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Name]) ->
  InitialState = ?ST_IDLE,
  {ok, InitialState, #atm_state{name = Name}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%state_name(_EventType, _EventContent, State = #atm_state{}) ->
%%  NextStateName = next_state,
%%  {next_state, NextStateName, State}.

state_idle(_EventType, {card_inserted, AccountNumber}, State = #atm_state{}) ->
  NextStateName = ?ST_GET_PIN,
  {next_state, NextStateName, State#atm_state{entered_pin = "", account_number = AccountNumber}};

state_idle(_EventType, stop, State = #atm_state{name = Name}) ->
  ok = atm:stop(Name),
  NextStateName = ?ST_IDLE,
  {next_state, NextStateName, State};

% unrecognized event under state ?ST_IDLE
state_idle(_EventType, _EventContent, State = #atm_state{}) ->
  KeepStateName = ?ST_IDLE,
  {next_state, KeepStateName, State}.


state_get_pin(_EventType, {digit, N}, State = #atm_state{entered_pin = CurrPin}) ->
  NewPin = CurrPin ++ integer_to_list(N),
  NextStateName = ?ST_GET_PIN,
  {next_state, NextStateName, State#atm_state{entered_pin = NewPin}};

state_get_pin(_EventType, enter, State = #atm_state{entered_pin = Pin, account_number = AccountNumber}) ->
  case backend:pin_valid(AccountNumber, Pin) of
    true ->
      NextStateName = ?ST_SELECTION,
      NewState = State;
    false ->
      % keep state. TODO: how to inform the user?
      NextStateName = ?ST_GET_PIN,
      NewState = State#atm_state{entered_pin = ""}
  end,
  {next_state, NextStateName, NewState};

state_get_pin(_EventType, clear, State = #atm_state{}) ->
  NewPin = "",
  NextStateName = ?ST_GET_PIN,
  {next_state, NextStateName, State#atm_state{entered_pin = NewPin}};

state_get_pin(_EventType, cancel, State = #atm_state{}) ->
  NextStateName = ?ST_IDLE,
  {next_state, NextStateName, clear_state(State)};

state_get_pin(_EventType, stop, State = #atm_state{name = Name}) ->
  ok = atm:stop(Name),
  NextStateName = ?ST_GET_PIN,
  {next_state, NextStateName, clear_state(State)};

% unrecognized event under state ?ST_GET_PIN
state_get_pin(_EventType, _EventContent, State = #atm_state{}) ->
  NextStateName = ?ST_GET_PIN,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%handle_event(_EventType, _EventContent, _StateName, State = #atm_state{}) ->
%%  NextStateName = the_next_state_name,
%%  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #atm_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #atm_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clear_state(State = #atm_state{}) ->
  State#atm_state{entered_pin = "", account_number = none}.