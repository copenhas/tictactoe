-module(ttt_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([connect/2, players/0, challenge/1, accept/1, decline/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ttt_playerdb.hrl").

-define(SERVER, ?MODULE).

-record(state, {players}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

connect(Name, Pid) when is_list(Name); is_pid(Pid) ->
    gen_server:call(?SERVER, {connect, Name, Pid}).

players() ->
    gen_server:call(?SERVER, players).

challenge(Name) when is_list(Name) ->
    gen_server:call(?SERVER, {challenge, Name}).

accept(Name) when is_list(Name) ->
    gen_server:call(?SERVER, {accept, Name}).

decline(Name) when is_list(Name) ->
    gen_server:call(?SERVER, {decline, Name}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        {ok, #state{
                players=ttt_playerdb:new()
            }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({connect, Name, Pid}, _From, State) ->
    case ttt_playerdb:get_by_name(State#state.players, Name) of
        #player{name=Name, pid=Pid} -> {reply, connected, State};
        #player{name=Name} ->
            {reply, {error, name_in_use}, State};
        undefined ->
            Mon = monitor(process, Pid),
            ttt_playerdb:add(State#state.players, 
                #player{name=Name, pid=Pid, ref=Mon}),
            {reply, connected, State}
    end;

handle_call(players, {Pid, _Ref}, State) ->
    Reply = run_if_connected(State, Pid, fun (_Player) ->
        Players = ttt_playerdb:players(State#state.players),
        [{N, S} || #player{name=N, status=S} <- Players]
    end),
    {reply, Reply, State};

handle_call({challenge, Name}, {Pid, _Ref}, State) ->
    Reply = run_if_connected(State, Pid, fun (Player) ->
        case ttt_playerdb:get_by_name(State#state.players, Name) of
            undefined -> {error, not_found};
            #player{pid=Challee} ->
                ttt_player:challenge(Challee, Player#player.name),
                issued
        end
    end),
    {reply, Reply, State};

handle_call({accept, Name}, {Pid, _Ref}, State) ->
    Reply = run_if_connected(State, Pid, fun(Player) ->
        case ttt_playerdb:get_by_name(State#state.players, Name) of
            undefined -> {error, not_found};
            #player{pid=Challenger} ->
                Game = ttt_game:start_link(Pid, Challenger),
                ttt_player:challenge_accepted(Challenger, Player#player.name, Game),
                Game
        end
    end),
    {reply, Reply, State};

handle_call({decline, Name}, {Pid, _Ref}, State) ->
    Reply = run_if_connected(State, Pid, fun(Player) ->
        case ttt_playerdb:get_by_name(State#state.players, Name) of
            undefined -> {error, not_found};
            #player{pid=Challenger} ->
                ttt_player:challenge_declined(Challenger, Player#player.name),
                ok
        end
    end),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    ttt_playerdb:remove_by_ref(State#state.players, Ref),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
        ttt_playerdb:delete(State#state.players),
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_if_connected(State, Pid, Cmd) ->
    case ttt_playerdb:get_by_pid(State#state.players, Pid) of
        undefined -> {error, not_connected};
        Connected -> Cmd(Connected)
    end.
