-module(player).

-include("ttt_player.hrl").
-behavior(ttt_player).

-export([create/1]).

-export([init/0, 
         challenge_recieved/2, 
         challenge_issued/2, 
         challenge_failed/3, 
         challenge_accepted/3,
         challenge_declined/2,
         game_started/3,
         turn/2,
         invalid_turn/3,
         turn_success/3,
         game_over/3
        ]).


create(Name) ->
    ttt_player:create(Name, ?MODULE).

init() -> ok.

% TODO: ok this is getting out of hand. what about a single handle_event or only a few functions
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
    io:fwrite("~p challenge accepted by ~p~n", [Info#info.name, Player]),
    Info.

challenge_declined(Player, Info) ->
    io:fwrite("~p challenge delcined by to ~p~n", [Info#info.name, Player]),
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

turn_success(_Game, Board, Info) ->
    io:fwrite("~p turn was made, ~p~n", [Info#info.name, Board]),
    Info.

game_over(_Game, Condition, Info) ->
    io:fwrite("~p ~p the game", [Info#info.name, Condition]),
    Info.
