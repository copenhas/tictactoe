
-record(info, {name, 
        challenges_pending=[],
        challenges_awaiting=[],
        callback,
        game=none,
        state=undefined,
        status=none}).
