-module(server).
-package(scrocht).
-author(kungdenknege@gmail.com).
-export([start/0, stop/0]).
-export([start/1]).

%%% Exported functions to start and stop server
start() ->
    register(server, spawn(?MODULE, start, [run])),
    ok.

stop() ->
    server ! exit,
    ok.

%%% Exported spawnable functions, for internal use only
start(run) ->
    process_flag(trap_exit, true),
    loop([]).

%%% Main message loop
loop(PidBrowserMap) ->
    receive
	{register, Pid, Browsers} -> %% Register 'client'
	    link(Pid),
	    NewPidBrowserMap = add(PidBrowserMap, Browsers, Pid),
	    loop(NewPidBrowserMap);
	{deregister, Pid} -> %% Deregister client
	    NewPidBrowserMap = del(PidBrowserMap, Pid),
	    loop(NewPidBrowserMap);
	{getlist, Pid} -> %% Send back a list of all available browsers (sort ot PID:s)
	    Pid ! getbrowsers(PidBrowserMap),
	    loop(PidBrowserMap);
	{shoot, Browser, Url, Email} -> %% Take a screenshot - look up associated PID and relay
	    case getpid(PidBrowserMap, Browser) of
		{ok, Pid} ->
		    Pid ! {shoot, Browser, Url, Email};
		false ->
		    erlog(node(), "Couldn't find browser", {Browser, Url, Email})
		end,
	    loop(PidBrowserMap);
	{'EXIT', Pid, Reason} -> %% Client died - log first, then restart and update PID's
	    erlog(node(Pid), "Node exited", Reason),
	    Browsers = getbrowsers(PidBrowserMap, Pid),
	    NewPidBrowserMap = del(PidBrowserMap, Pid),
	    %% TODO the following must be checked for exceptions. If we can't restart we have to deregister!
	    NewPid = spawn_link(node(Pid), scrochter, wakeup, [node()]), %% Not pretty, should be able to handle other modules as clients as well
	    NewNewPidBrowserMap = add(NewPidBrowserMap, Browsers, NewPid),
	    loop(NewNewPidBrowserMap);
	exit ->
	    ok;
	Unexpected ->
	    erlog(node(), "Unexpected mess", Unexpected),
	    loop(PidBrowserMap)
    end.

%%% Local functions to manipulate the list that maps browsers to pids
add(PidBrowserMap, [], _Pid) ->
    PidBrowserMap;

add(PidBrowserMap, [BrowserHead | BrowserTail], Pid) ->
    add([{BrowserHead, Pid} | PidBrowserMap], BrowserTail, Pid).

del(PidBrowserMap, Pid) ->
    del(PidBrowserMap, [], Pid).

del([], Processed, _Pid) ->
    Processed;

del([{_Browser, Pid} | BrowserMapTail], Processed, Pid) ->
    del(BrowserMapTail, Processed, Pid);

del([BrowserMapHead | BrowserMapTail], Processed, Pid) ->
    del(BrowserMapTail, [BrowserMapHead | Processed], Pid).

getbrowsers([{Browser, _Pid} | Tail]) ->
    [Browser | getbrowsers(Tail)]; %% Perhaps this isn't tail recursive? But pretty!

getbrowsers([]) ->
    [].

getbrowsers(BrowserMap, Pid) ->
    getbrowsers(BrowserMap, Pid, []).

getbrowsers([], _Pid, Result) ->
    Result;

getbrowsers([{Browser, Pid} | Tail], Pid, Result) ->
    getbrowsers(Tail, Pid, [Browser | Result]);

getbrowsers([_Head | Tail], Pid, Result) ->
    getbrowsers(Tail, Pid, Result).

getpid([{Browser, Pid} | _Tail], Browser) ->
    {ok, Pid};

getpid([_Head | Tail], Browser) ->
    getpid(Tail, Browser);

getpid([], _Browser) ->
    false.

%% Error handling
erlog(Node, Msg, Data) ->
    io:fwrite("Node: ~w~n Msg: ~s~n Data: ~w~n~n", [Node, Msg, Data]).
