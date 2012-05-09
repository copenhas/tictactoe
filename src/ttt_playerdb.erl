-module(ttt_playerdb).

-include("ttt_playerdb.hrl").

-export([new/0, delete/1]).
-export([add/2, players/1, 
         get_by_name/2, get_by_ref/2, get_by_pid/2,
         remove_by_name/2, remove_by_ref/2, remove_player/2]).

-record(playerdb, {ref2name, pid2name, name2player}).

new() ->
    #playerdb{ref2name=ets:new(ref2name, []), 
              pid2name=ets:new(pid2name, []),
              name2player=ets:new(name2player, [{keypos, 2}])}.


delete(Db) ->
    ets:delete(Db#playerdb.ref2name),
    ets:delete(Db#playerdb.pid2name),
    ets:delete(Db#playerdb.name2player).


add(Db, Player=#player{name=Name, pid=Pid, ref=Mon}) when is_record(Player, player) ->
    ets:insert(Db#playerdb.name2player, Player),
    ets:insert(Db#playerdb.ref2name, {Mon, Name}),
    ets:insert(Db#playerdb.pid2name, {Pid, Name}).


players(Db) ->
    ets:tab2list(Db#playerdb.name2player).


get_by_name(Db, Name) when is_list(Name) ->
    case ets:lookup(Db#playerdb.name2player, Name) of
        [] -> undefined;
        [Player] -> Player
    end.


get_by_ref(Db, Mon) when is_reference(Mon) ->
    case ets:lookup(Db#playerdb.ref2name, Mon) of
        [] -> undefined;
        [{Mon, Name}] -> get_by_name(Db, Name)
    end.



get_by_pid(Db, Pid) when is_pid(Pid) ->
    case ets:lookup(Db#playerdb.pid2name, Pid) of
        [] -> undefined;
        [{Pid, Name}] -> get_by_name(Db, Name)
    end.

remove_by_name(Db, Name) when is_list(Name) ->
    Player = get_by_name(Db, Name),
    remove_player(Db, Player).


remove_by_ref(Db, Mon) when is_reference(Mon) ->
    Player = get_by_ref(Db, Mon),
    remove_player(Db, Player).


remove_player(Db, Player) when is_record(Player, player) ->
    case Player of
        undefined -> true;
        #player{name=Name, ref=Mon} ->
            ets:delete(Db#playerdb.name2player, Name),
            ets:delete(Db#playerdb.ref2name, Mon)
    end.
