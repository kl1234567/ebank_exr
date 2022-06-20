%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : webatm.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2017 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(webatm).
-vsn('1.0').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("kernel/include/inet.hrl").
-include_lib("ebank/include/backend.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%
% Management Interface
%
-export([start/2, start_link/2, stop/1]).

%%%%%
% Webpage callbacks
%
-export([do/3]).

%%%%%
% User API
%
-export([cancel/1,
         clear/1,
         show_balance/2,
         show_mini_statement/3,
         show_pin_valid_message/1,
         show_pin_invalid_message/1,
         show_input/2,
         show_withdraw_message/1,
         show_successful_withdraw/1,
         show_unsuccessful_withdraw/2,
         show_pin_request/1,
         show_timeout/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SERVER_ADDR, {127,0,0,1}).
-define(HTTP_SERVER, "http://127.0.0.1").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MANAGEMENT INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Port) ->
    PrivDir = code:priv_dir(ebank_frontend),
    DocumentRootDir = filename:join([PrivDir, "www"]),
    Conf = [
        {port, Port},
        {server_name, atom_to_list(Name)},
        {bind_address, ?SERVER_ADDR},
        {server_root, PrivDir}, % logs
        {document_root, DocumentRootDir}, % files
        {modules, [mod_alias, mod_auth, mod_esi, mod_actions,
                   mod_get, mod_head, mod_log, mod_trace]},
        {error_log, "logs/atm_error_log.txt"},
        {security_log, "logs/atm_security_log.txt"},
        {transfer_log, "logs/atm_transfer_log.txt"},
        {directory_index, ["atm.html"]},
        {erl_script_alias, {"/atm", [?MODULE]}}
    ],
    io:format("Visit ~s:~p to see the ATM. Javascript must be enabled.~n", [?HTTP_SERVER, Port]),
    {ok, Pid} = inets:start(httpd, Conf, stand_alone),
    make_name(Name, Port, undefined),
    {ok, Pid}.

start_link(Name, Port) ->
    {ok, Pid} = start(Name, Port),
    link(Pid),
    {ok, Pid}.

stop(Name) ->
    Pid = lookup_pid(Name),
    delete_name(Name),
    inets:stop(httpd, Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do(Name, ListOfCmds) ->
    lookup_pid(Name) ! {'#do', ListOfCmds},
    ok.

display(Txt) -> {display, Txt}.

append_line(Txt) -> {append_line, Txt}.

%%append(Txt) -> {append, Txt}.

high_light(off) -> {highlight, off};
high_light(Section) -> {highlight, Section}.

wait(Duration) -> {wait, Duration}.

eject() -> {eject, "eject"}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HTTPD CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cancel(Name) ->
    do(Name, [
              display("cancel: Cancel button pressed"),
              eject()
             ]).

clear(Name) ->
    do(Name, [
              display(" ")
             ]).

show_balance(Name, Balance) ->
    do(Name, [
              high_light("balance"),
              display("Balance:"),
              append_line("-------------------------"),
              append_line(io_lib:format("£ ~p", [Balance]))
             ]).

show_mini_statement(Name, Transactions, Balance) ->
    ActionList =
        lists:map(
          fun({Type, {{Year, Month, Day}, {Hour, Minute, Second}}, Sum}) ->
                  Con = case Type of
                            credit -> "";
                            debit -> "-"
                        end,
                  append_line(
                    io_lib:format("~p/~p/~p ~p:~p~p ~s ~p~n",
                                  [Day, Month, Year, Hour, Minute, Second, Con, Sum]))
          end,
          lists:sublist(Transactions, 1, 10)),
    do(Name, [
              high_light("statement"),
              display("Mini Statement:"),
              append_line("---------------------"),
              ActionList,
              append_line(io_lib:format("Balance: £ ~p", [Balance]))
             ]).

show_pin_valid_message(Name) ->
    do(Name, [display("Please make your selection")]).

show_pin_invalid_message(Name) ->
    do(Name, [
              display("PIN code incorrect!"),
              append_line("Please try again.")
             ]).

show_input(Name, Digits) ->
    do(Name, [display(Digits)]).

show_withdraw_message(Name) ->
    do(Name, [
              high_light("withdraw"),
              display("How much would you like to withdraw?")
             ]).

show_successful_withdraw(Name) ->
    do(Name, [
              display("Take the money and run."),
              wait(3500),
              high_light("off"),
              eject()
             ]).

show_unsuccessful_withdraw(Name, Reason) ->
    do(Name, [
              display("Could not withdraw money!"),
              append_line(io_lib:format("~p",[Reason])),
              wait(3500),
              high_light("off"),
              eject()
             ]).

show_pin_request(Name) ->
    do(Name, [display("Please type your PIN code")]).

show_timeout(Name) ->
    do(Name, [
              display("Session Timed Out."),
              wait(3500),
              eject()
    ]).


do(SessId, Env, Input) ->
    Port = proplists:get_value(server_port, Env),
    update_pid(Port, self()),
    Name = lookup_name(Port),
    run(SessId, Input, Name).

%% Fetches cards
run(SessId, "start/"++QueryString, _Name) ->
    Accounts = backend:list_accounts(),
    Cmds = [{account, [integer_to_list(Number),$: | Name]}
             || #account{acc_no = Number, name = Name} <- Accounts],
    Callback = proplists:get_value("callback", httpd:parse_query(QueryString)),
    jsonp(SessId, Callback, Cmds);

%% User picked a card
run(SessId, "event/card/"++Rest, Name) ->
    [Num, QueryString] = re:split(Rest, "\/", [{return, list}]),
    atm:card_inserted(Name, list_to_integer(Num)),
    Reply = get_cmds(),
    Callback = proplists:get_value("callback", httpd:parse_query(QueryString)),
    jsonp(SessId, Callback, Reply);

%% User pressed a button
run(SessId, "event/"++Rest, Name) ->
    [Button, QueryString] = re:split(Rest, "\/", [{return, list}]),
    try list_to_integer(Button) of
        _ -> % digit button pressed
            atm:event(Name, {digit, Button})
    catch
        error:badarg -> % other button!
            case Button of
                "withdraw" -> atm:event(Name, {selection, withdraw});
                "balance" -> atm:event(Name, {selection, balance});
                "statement" -> atm:event(Name, {selection, statement});
                _ -> atm:event(Name, list_to_atom(Button))
            end
    end,
    Reply = get_cmds(),
    Callback = proplists:get_value("callback", httpd:parse_query(QueryString)),
    jsonp(SessId, Callback, Reply);


run(SessId, _Args, _Name) ->
    jsonp(SessId, "fsm.run", [{append_line, "Unexpected Query"}]).

headers(SessId, Data) ->
    Headers = [[[Key,": ", Val, "\r\n"] || {Key, Val} <- Data], "\r\n"],
    mod_esi:deliver(SessId, lists:flatten(Headers)).

body(SessId, Data) ->
    mod_esi:deliver(SessId, Data).

jsonp(SessId, Callback, Val) ->
    headers(SessId, [{"Content-Type", "text/javascript"}]),
    Vals = [["{'command':'",atom_to_list(Command), "', 'val':'", fix(Str), "'},"]
             || {Command, Str} <- Val],
    JSON = "["++string:strip(lists:flatten(Vals), right, $,)++"]",
    body(SessId, [Callback, $(, JSON, $), $;]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_cmds() ->
    receive
        {'#do', ListOfCmds} -> lists:flatten(ListOfCmds)
    end.

fix(N) when is_integer(N) -> integer_to_list(N);
fix(Str) ->
    re:replace(Str, "\n", "", [global, {return, list}]).

make_name(Name, Port, Pid) when is_atom(Name) ->
    try
        ets:new(?MODULE, [named_table, public, {heir, whereis(application_controller), permanent}])
    catch
        error:badarg -> ok  % table exists
    end,
    ets:insert(?MODULE, {Name, Port, Pid}),
    ok.

lookup_name(Port) when is_integer(Port) ->
    [[Name]] = ets:match(?MODULE, {'$1', Port, '_'}),
    Name.

lookup_pid(Name) when is_atom(Name) ->
    [[Pid]] = ets:match(?MODULE, {Name, '_', '$1'}),
    Pid.

update_pid(Port, Pid) when is_integer(Port) ->
    [[Name]] = ets:match(?MODULE, {'$1', Port, '_'}),
    ets:insert(?MODULE, {Name, Port, Pid}),
    ok.

delete_name(Name) when is_atom(Name) ->
    ets:delete(?MODULE, Name),
    ok.

