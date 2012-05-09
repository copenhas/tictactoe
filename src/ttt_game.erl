-module(ttt_game).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).
-export([start/1, get_board/1, move/2]).

%% gen_fsm callbacks
-export([init/1,
         created/2,
         player_turn/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {
        players=[],
        current=x,
        board
    }).

%%%===================================================================
%%% API
%%%===================================================================

start(Game) when is_pid(Game) ->
    gen_fsm:send_event(Game, start).

get_board(Game) when is_pid(Game) ->
    gen_fsm:sync_send_all_state_event(Game, get_board).

move(Game, Coords) when is_pid(Game) ->
    gen_fsm:sync_send_event(Game, {place, Coords}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Player1, Player2) ->
    gen_fsm:start_link(?SERVER, [Player1, Player2], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Player1, Player2]) ->
    {ok, created, #state{
            players=[{x, Player1}, {o, Player2}],
            board=ttt_board:new()
        }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
created(start, State) ->
    ttt_player:turn(current_player(State), self()),
    {next_state, player_turn, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
player_turn({place, Coords}, {Pid, _Ref}, State) ->
    Funcs = [
        fun check_player/1,
        fun update_board/1, 
        fun check_victory/1
    ],
    case pipe({Pid, State, Coords}, Funcs) of
        {error, Reason, _Func} -> 
            {reply, {error, Reason}, player_turn, State};
        {player_turn, UpdatedState} ->
            CurrentPlayer = current_player(UpdatedState),
            ttt_player:turn(CurrentPlayer, self()),
            {reply, {ok, UpdatedState#state.board}, player_turn, UpdatedState};
        {Winner, UpdatedState} ->
            ttt_player:game_over(other_player(UpdatedState), self(), lose),
            {stop, Winner, {ok, UpdatedState#state.board}, UpdatedState}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(none, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(get_board, _From, StateName, State) ->
    {reply, State#state.board, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(none, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
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

pipe(Args, []) -> Args;

pipe(Args, [Func|Tail]) ->
    case apply(Func, [Args]) of
        {error, Reason} ->
            {error, Reason, Func};
        ok ->
            pipe([], Tail);
        Next ->
            pipe(Next, Tail)
    end.

check_player({Pid, State, Move}) ->
    Current = State#state.current,
    case lists:keyfind(Pid, 2, State#state.players) of
        {Current, Pid} -> {State, Current, Move};
        _ -> {error, not_current_player} 
    end.

update_board({State, Piece, Move}) ->
    case ttt_board:place(State#state.board, Piece, Move) of
        {error, Reason} -> {error, Reason};
        NewBoard ->
            State#state{board = NewBoard}
    end.

check_victory(State) ->
    Current = State#state.current,
    case ttt_board:victory(State#state.board) of
        none ->
            {player_turn, State#state{current = next_player(State)}};
        Winner = {winner, Current} ->
            {Winner, State}
    end.

next_player(State) ->
    case State#state.current of
        x -> o;
        o -> x
    end.

current_player(State) ->
    {_Piece, Player} = lists:keyfind(State#state.current, 1, State#state.players),
    Player.

other_player(State) ->
    {_Piece, Player} = hd(lists:filter(
        fun ({Piece, _Pid}) -> Piece =/= State#state.current end, 
        State#state.players)),
    Player.
