-module(ttt_player).

-include("ttt_player.hrl").

%% custom behavior API
-export([behaviour_info/1]).

%% behavior common API
-export([create/2,
         issue_challenge/2,
         accept_challenge/2,
         decline_challenge/2,
         get_players/1,
         move/3]).

% Public API
-export([
        game_over/3,
        challenge/2,
        challenge_declined/2,
        challenge_accepted/3,
        turn/2
    ]).


-behavior(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API 
%%%===================================================================
game_over(Player, Game, Condition) when is_pid(Player), is_pid(Game) ->
    gen_server:cast(Player, {game_over, Game, Condition}).

challenge(Player, Challenger) ->
    gen_server:cast(Player, {challenge_recieved, Challenger}).

challenge_declined(Player, Name) ->
    gen_server:cast(Player, {challenge_declined, Name}).

challenge_accepted(Player, Name, Game) ->
    gen_server:cast(Player, {challenge_accepted, Name, Game}).

turn(Player, Game) ->
    gen_server:cast(Player, {turn, Game}).

%%%===================================================================
%%% Behavior API
%%%===================================================================
create(Name, Callback) ->
    {ok, Pid} = gen_server:start(?MODULE, [Name, Callback], []),
    Pid.

accept_challenge(Pid, Challenger) when is_pid(Pid), is_list(Challenger) ->
    gen_server:cast(Pid, {accept_challenge, Challenger}).

decline_challenge(Pid, Challenger) when is_pid(Pid), is_list(Challenger) ->
    gen_server:cast(Pid, {decline_challenge, Challenger}).

issue_challenge(Pid, Name) when is_pid(Pid), is_list(Name) ->
    gen_server:cast(Pid, {issue_challenge, Name}).

get_players(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, players).

move(Pid, Game, Coords = {X, Y}) when is_pid(Pid), is_pid(Game), is_integer(X), is_integer(Y) ->
    gen_server:cast(Pid, {place, Game, Coords}).

%%%===================================================================
%%% Behavior Exports
%%%===================================================================
behaviour_info(callbacks) ->
    [{init, 0},
     {challenge_recieved, 2}, % (Player, Info)
     {challenge_issued, 2}, % (Player, Info)
     {challenge_failed, 3}, % (Player, Reason, Info)
     {challenge_accepted, 3}, % (Player, Game, Info)
     {challenge_declined, 2}, % (Player, Info)
     {game_started, 3}, % (Player, Game, Info)
     {turn, 2}, % (Game, Info)
     {invalid_turn, 3}, % (Game, Move, Info)
     {turn_success, 3}, % (Game, Board, Info)
     {game_over, 3} % (Game, win|lose, Info)
    ]; 

behaviour_info(_)->
    undefined.


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
    {noreply, Next};

handle_cast({challenge_declined, Player}, Info) ->
    Challenges = Info#info.challenges_pending,
    Callback = Info#info.callback,
    NewInfo = Info#info{challenges_pending = lists:delete(Player, Challenges)},
    Next = Callback:challenge_declined(Player, NewInfo),
    {noreply, Next};

handle_cast({challenge_accepted, Player, Game}, Info) ->
    Challenges = Info#info.challenges_pending,
    Callback = Info#info.callback,
    NewInfo = Info#info{challenges_pending = lists:delete(Player, Challenges),
                        game = Game},
    Next = Callback:challenge_accepted(Player, Game, NewInfo),
    {noreply, Next};

handle_cast({challenge_recieved, Player}, Info) ->
    Challenges = Info#info.challenges_awaiting,
    Callback = Info#info.callback,
    NewInfo = Info#info{challenges_awaiting = [Player | Challenges]},
    Next = Callback:challenge_recieved(Player, NewInfo),
    {noreply, Next};

handle_cast({accept_challenge, Name}, Info) ->
    Challenges = Info#info.challenges_awaiting,
    Callback = Info#info.callback,
    Game = ttt_server:accept(Name),
    NewInfo = Info#info{game = Game, challenges_awaiting = lists:delete(Name, Challenges)},
    Next = Callback:game_started(Name, Game, NewInfo),
    {noreply, Next}; 

handle_cast({decline_challenge, Name}, Info) ->
    Challenges = Info#info.challenges_awaiting,
    ttt_server:decline(Name),
    NewInfo = Info#info{challenges_awaiting = lists:delete(Name, Challenges)},
    {noreply, NewInfo};

handle_cast({turn, Game}, Info) ->
    Callback = Info#info.callback,
    NewInfo = Callback:turn(Game, Info),
    {noreply, NewInfo};

handle_cast({place, Game, Coords}, Info) ->
   Callback = Info#info.callback,
   NewInfo = case ttt_game:move(Game, Coords) of
        {ok, Board} ->
            Callback:turn_success(Game, Board, Info);
        {error, invalid_turn} ->
            Callback:invalid_turn(Game, Coords, Info);
        {winner, _Player} ->
            Callback:game_over(Game, win, Info)
   end,
   {noreply, NewInfo};

handle_cast({game_over, Game, Condition}, Info) ->
    Callback = Info#info.callback,
    Updated = Info#info{game=none},
    NewInfo = Callback:game_over(Game, Condition, Updated),
    {noreply, NewInfo}.

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

