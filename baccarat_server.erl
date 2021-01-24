%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jun 2018 5:31 PM
%%%-------------------------------------------------------------------
-module(baccarat_server).
-author("ysx").

-behaviour(gen_statem).

-include("sys_log.hrl").
-include("db.hrl").

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([
    init/1,
    format_status/2,
    handle_event/4,
    terminate/3,
    code_change/4,
    callback_mode/0
]).

%% state_function
-export([down/3, open/3, bet/3, test/3, draw/3, sync/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Object) ->
    gen_statem:start_link(?MODULE, [Object], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([RoomID]) ->
    process_flag(trap_exit, true),
    [Object] = mnesia:dirty_read(baccarat, RoomID),
    #baccarat{room_id = RoomID, owner = AgentID} = Object,
    State = baccarat_init:init(Object),
    baccarat_mgr:register(RoomID, self(), AgentID),
    {ok, down, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
    [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%%state_name(_EventType, _EventContent, State) ->
%%    NextStateName = next_state,
%%    {next_state, NextStateName, State}.

down(cast, {mfa, _}, Data) ->
    {keep_state, Data};
down(cast, NextState, Data) ->
    {next_state, NextState, Data};
down(info, test, Data) ->
    {keep_state, Data};
down({call, From}, test, Data) ->
    {keep_state, Data, [{reply, From, ok}]};
down(enter, _OldState, Data) ->
    baccarat_status:downing(Data),
    {keep_state, Data};
down(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, down, Data).

open(enter, _OldState, Data) ->
    case baccarat_status:opening(Data) of
        {NewData, Action} ->
            {keep_state, NewData, [Action]};
        _->
            {keep_state, Data}
    end;
%%    {NewData, Action} = baccarat_status:opening(Data),
%%    {keep_state, NewData, [Action]};
open(info, auth_printer, Data) ->
    {NewData, Action} = baccarat_status:auth(Data),
    {keep_state, NewData, [Action]};
open(info, reprint, Data) ->
    baccarat_callback:reprint(Data),
    {keep_state, Data};
open(state_timeout, NextState, Data) ->
    {next_state, NextState, Data};
open(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, open, Data).

bet(enter, _OldState, Data) ->
    {NewData, Action} = baccarat_status:betting(Data),
    {keep_state, NewData, [Action]};
bet(state_timeout, NextState, Data) ->
    {next_state, NextState, Data};
bet(info, {timeout, _Ref, update_bets}, Data) ->
    baccarat_logic:update_bets(),
    {keep_state, Data};
bet(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, bet, Data).

test(enter, _OldState, Data) ->
    {NewData, Action} = baccarat_status:testing(Data),
    {keep_state, NewData, [Action]};
test(state_timeout, draw, Data) ->
    {next_state, draw, Data};
test(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, test, Data).

draw(enter, _OldState, Data) ->
    {NewData, Action} = baccarat_status:drawing(Data),
    {keep_state, NewData, [Action]};
draw(state_timeout, NextState, Data) ->
    {next_state, NextState, Data};
draw(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, draw, Data).

sync(enter, _OldState, Data) ->
    {NewData, Action} = baccarat_status:syncing(Data),
    {keep_state, NewData, [Action]};
sync(state_timeout, NextState, Data) ->
    {next_state, NextState, Data};
sync(info, {sync_table_bet, {PlayerBets, GlobalBets, Test}}, Data) ->
    baccarat_callback:sync_table_bet(PlayerBets, GlobalBets, Test),
    {next_state, test, Data};
sync(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, sync, Data).

%%auth(enter, _OldState, Data) ->
%%    {keep_state, Data};
%%auth(info, auth_printer, Data) ->
%%    {next_state, open, Data};
%%auth(EventType, EventContent, Data) ->
%%    all_state(EventType, EventContent, sync, Data).

all_state(cast, {mfa, {M, F, A}}, _Status, Data) ->
    erlang:apply(M, F, A),
    {keep_state, Data};

all_state(cast, stop, Status, Data) ->
    NewStatus = baccarat_callback:stop(Status),
    {next_state, NewStatus, Data};

all_state({call, From}, {mfa, {M, F, A}}, Status, Data) ->
    Reply = erlang:apply(M, F, [Data,Status|A]),
    {keep_state, Data, [{reply, From, Reply}]};

all_state({call, From}, {mfa_with_state, {M, F, A}}, Status, Data) ->
    {Reply, NewData} = erlang:apply(M, F, [Data,Status|A]),
    {keep_state, NewData, [{reply, From, Reply}]};

all_state({call, From}, get_status, Status, Data) ->
    {keep_state, Data, [{reply, From, {ok, Status}}]};

all_state(_EventType, _EventContent, _Status, Data) ->
%%    ?DEBUG("EventType:~p, EventContent:~p, Status:~p~n", [_EventType, _EventContent, _Status]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
    {keep_state, State}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    sync_data(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sync_data() ->
    Setting = #baccarat{room_id = RoomID, round = Round} = baccarat_dict:get_setting(),
    case mnesia:dirty_read(baccarat, RoomID) of
        [O] ->
            mnesia:dirty_write(O#baccarat{round = Round});
        _ ->
            mnesia:dirty_write(Setting)

    end,
    Accounting = baccarat_dict:get_accounting(),
    mnesia:dirty_write(Accounting).



