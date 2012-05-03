-module(player).

-include("ttt_player.hrl").
-behavior(ttt_player).

-export([create/1, 
         challenge/2, 
         players/1,
         accept/2,
         decline/2
        ]).

-export([init/0, 
         challenge_recieved/2, 
         challenge_issued/2, 
         challenge_failed/3, 
         challenge_accepted/3,
         game_started/3,
         turn/2,
         invalid_turn/3,
         won/2,
         lost/2
        ]).


create(Name) ->
    ttt_player:create(Name, ?MODULE).

challenge(Pid, Name) when is_pid(Pid) ->
    ttt_player:issue_challenge(Pid, Name).

accept(Pid, Name) ->
    ttt_player:accept_challenge(Pid, Name).

decline(Pid, Name) ->
    ttt_player:decline_challenge(Pid, Name).

players(Pid) when is_pid(Pid) ->
    ttt_player:get_players(Pid).


init() -> ok.

challenge_recieved(_Player, Info) ->
    io:fwrite("~p recieved challenge~n", [Info#info.name]),
    Info.

challenge_issued(Player, Info) ->
    io:fwrite("~p challenge issued to ~p~n", [Info#info.name, Player]),
    Info.

challenge_failed(Player, _Reason, Info) ->
    io:fwrite("~p challenge failed to ~p~n", [Info#info.name, Player]),
    Info.

challenge_accepted(Player, _Game, Info) ->
    io:fwrite("~p challenge accepted to ~p~n", [Info#info.name, Player]),
    Info.

game_started(Player, _Game, Info) ->
    io:fwrite("~p game started with ~p~n", [Info#info.name, Player]),
    Info.

turn(_Game, Info) ->
    io:fwrite("~p's turn~n", [Info#info.name]),
    Info.

invalid_turn(_Game, Move, Info) ->
    io:fwrite("~p turn was invalid, ~p~n", [Info#info.name, Move]),
    Info.

won(_Game, Info) ->
    io:fwrite("~p won~n", [Info#info.name]),
    Info.

lost(_Game, Info) ->
    io:fwrite("~p lost~n", [Info#info.name]),
    Info.