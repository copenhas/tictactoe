-module(ttt_player).

-include("ttt_player.hrl").

%% custom behavior API
-export([behaviour_info/1]).

%% behavior common API
-export([create/2,
         recieve_challenge/2, issue_challenge/2,
         get_players/1]).


-behavior(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%%%===================================================================
%%% Behavior Exports
%%%===================================================================
behaviour_info(callbacks) ->
    [{init, 0},
     {challenge_recieved, 2},
     {challenge_issued, 2},
     {challenge_failed, 3},
     {challenge_accepted, 3}];

behaviour_info(_)->
    undefined.


%%%===================================================================
%%% API
%%%===================================================================

create(Name, Callback) ->
    {ok, Pid} = gen_server:start(?MODULE, [Name, Callback], []),
    Pid.

recieve_challenge(Pid, Challenger) when is_pid(Pid); is_list(Challenger) ->
    gen_server:cast(Pid, {challenge, Challenger}).

issue_challenge(Pid, Name) when is_pid(Pid) ->
    gen_server:cast(Pid, {issue_challenge, Name}).

get_players(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, players).


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
init([Name, Callback]) ->
    State = Callback:init(),
    case ttt_server:connect(Name, self()) of
        {error, Reason} -> throw({error, Reason});
        connected -> ok
    end,
    {ok, #info{name=Name, callback=Callback, status=connected, state=State}}.

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
handle_call(players, _From, Info) ->
    {reply, ttt_server:players(), Info}.

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
handle_cast({challenge, Player}, Info) ->
    Challenges = Info#info.challenges_awaiting,
    Callback = Info#info.callback,
    NewInfo = Info#info{challenges_awaiting = [Player | Challenges]},
    Next = Callback:challenge_recieved(Player, NewInfo),
    {noreply, Next};
    
handle_cast({issue_challenge, Name}, Info) ->
    Challenges = Info#info.challenges_pending,
    Callback = Info#info.callback,
    Next = case ttt_server:challenge(Name) of
        issued ->
            NewInfo = Info#info{challenges_pending = [Name | Challenges]},
            Callback:challenge_issued(Name, NewInfo);
        {error, Reason} ->
            Callback:challenge_failed(Name, Reason, Info)
    end,
    {noreply, Next}.

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
handle_info(none, State) ->
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
terminate(_Reason, _State) ->
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

