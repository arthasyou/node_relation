%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jun 2018 3:08 PM
%%%-------------------------------------------------------------------
-module(baccarat_status).
-author("ysx").
-include("baccarat.hrl").
-include("sys_log.hrl").
-include("db.hrl").

%% ==================================================
%% API
%% ==================================================
-export([opening/1, betting/1, testing/1, drawing/1, downing/1]).
-export([syncing/1]).
-export([auth/1]).

%% 开局
opening(State) ->
%%    ?DEBUG("going here: ~p~n", [opening]),
    baccarat_logic:opening(State),
    status_auth(State).
%%    status_opening(State).

betting(State) ->
%%    ?DEBUG("going here: ~p~n", [betting]),
    baccarat_logic:betting(State),
    status_betting(State).
%%    case auth_insurance() of
%%        false ->
%%            status_betting(State);
%%        Reason ->
%%            %% TODO 对玩家记录进行备份
%%            status_down(State, Reason)
%%    end.

testing(State) ->
%%    ?DEBUG("going here: ~p~n", [testing]),
    baccarat_logic:testing(State),
    status_testing(State).

drawing(State) ->
    {Msg, DrawingTime} = baccarat_logic:drawing(State),
    status_drawing(State, Msg, DrawingTime).

downing(State) ->
    baccarat_logic:downing(State),
    ok.

syncing(State) ->
    #baccarat_state{table_pid = PID} = State,
    baccarat_notice:send_syncing(PID),
    status_syncing(State).

auth(State) ->
    Setting = baccarat_dict:get_setting(),
    #baccarat{
%%        opening_time = OpenTime
%%        print_is_open = PrintFlag
    } = Setting,

%%    Msg =
%%        case PrintFlag of
%%            1 ->
%%                down;
%%            2 ->
%%                down;
%%            _ ->
%%                bet
%%        end,

%%    TimeRef = erlang:start_timer(1000, self(), bet),
    OpenTime = 10,
    EndTime = time:now() + OpenTime,
    NewState = State#baccarat_state{
        end_time = EndTime
    },
    Action = {state_timeout, OpenTime*1000, bet},
    {NewState, Action}.

%% ==================================================
%% Internal
%% ==================================================

status_opening(State) ->
    Setting = baccarat_dict:get_setting(),
    #baccarat{
        opening_time = OpenTime
%%        print_is_open = PrintFlag
    } = Setting,

%%    Msg =
%%        case PrintFlag of
%%            1 ->
%%                down;
%%            2 ->
%%                down;
%%            _ ->
%%                bet
%%        end,

%%    TimeRef = erlang:start_timer(1000, self(), bet),
    EndTime = time:now() + OpenTime,
    NewState = State#baccarat_state{
        end_time = EndTime
    },
    Action = {state_timeout, OpenTime*1000, bet},
    {NewState, Action}.

status_betting(State = #baccarat_state{id = RoomID}) ->
    Setting = baccarat_dict:get_setting(),
    #baccarat{betting_time = BettingTime} = Setting,
    Action = {state_timeout, (BettingTime-3)*1000, sync},
    EndTime = time:now() + BettingTime,
    baccarat_mgr:update(RoomID, EndTime),
    baccarat_logic:update_baccarat_room(RoomID),
    NewState = State#baccarat_state{
        end_time = EndTime
    },
    {NewState, Action}.

status_testing(State) ->
    Action = {state_timeout, ?TESTING_TIME, draw},
    EndTime = time:now() + (?TESTING_TIME) div 1000,
    NewState = State#baccarat_state{
        end_time = EndTime
    },
    {NewState, Action}.

status_drawing(State, Msg, DrawingTime) ->
    Action = {state_timeout, DrawingTime*1000, Msg},
    EndTime = time:now() + DrawingTime,
    NewState = State#baccarat_state{
        end_time = EndTime
    },
    {NewState, Action}.

status_syncing(State) ->
    Action = {state_timeout, 1000, test},
    EndTime = time:now() + (?TESTING_TIME) div 1000,
    NewState = State#baccarat_state{
        end_time = EndTime
    },
    {NewState, Action}.

status_auth(State) ->
    case baccarat_dict:get_setting() of
        #baccarat{print_flag = 0} ->
            status_opening(State);
        _ ->
            ok
    end.


%%status_down(State) ->
%%    TimeRef = erlang:start_timer(?DRAWING_TIME, self(), down),
%%    State#baccarat_state{
%%        end_time = time:now() + (?DRAWING_TIME) div 1000,
%%        time_ref = TimeRef
%%    }.
