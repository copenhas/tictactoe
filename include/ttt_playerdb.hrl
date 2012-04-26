
-record(player, {name, % string()
                 pid, % pid()
                 ref, % ref()
                 status=idle}). % atom() | tuple() - idle, {challenging, PlayerName}, {in_game, PlayerName}
