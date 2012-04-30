-module(player).

-include("ttt_player.hrl").
-behavior(ttt_player).

-export([create/1, challenge/2, players/1]).

-export([init/0, 
         challenge_recieved/2, challenge_issued/2, 
         challenge_failed/3, challenge_accepted/3]).


create(Name) ->
    ttt_player:create(Name, ?MODULE).

challenge(Pid, Name) when is_pid(Pid) ->
    ttt_player:issue_challenge(Pid, Name).

players(Pid) when is_pid(Pid) ->
    ttt_player:get_players(Pid).


init() -> ok.

challenge_recieved(_Player, Info) ->
    io:fwrite("~p recieved challenge", [self()]),
    Info.

challenge_issued(Player, Info) ->
    io:fwrite("~p challenge issued to ~p", [self(), Player]),
    Info.

challenge_failed(Player, _Reason, Info) ->
    io:fwrite("~p challenge failed to ~p", [self(), Player]),
    Info.

challenge_accepted(Player, _Game, Info) ->
    io:fwrite("~p challenge accepted to ~p", [self(), Player]),
    Info.
