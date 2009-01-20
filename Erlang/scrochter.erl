-module(scrochter).
-package(scrocht).
-author(kungdenknege@gmail.com).
-export([start/1, stop/1]).
-export([loop/2, wakeup/1, wakeup/2]).
-define(CONFIG_FILE, scrochter.conf).
-define(MAXMAILTRIES, 3).

start(Node) ->
    Pid = spawn(?MODULE, wakeup, [Node, Config = config()]),
    {server, Node} ! {register, Pid, browsers(Config)},
    Pid.

stop(Pid) ->
    Pid ! exit,
    ok.

wakeup(Node) ->
    wakeup(Node, config()).

wakeup(Node, Config) ->
    {I1, I2, I3} = now(),
    random:seed(I1, I2, I3),
    loop(Node, Config).

loop(Node, Config) ->
    receive
	exit ->
	    {server, Node} ! {deregister, self()},
	    ok;
	{shoot, Browser, Url, Email} ->
	    shoot(Browser, Url, Email, Config),
	    loop(Node, Config)
    end.


%% Actually take screenshot with speced browser and resolution	
shoot({Os, Browser, Resolution}, Url, Email, Config) ->
    BrowserExec = getexec(Browser, Config),
    {value, {"Executable", Executable}} = lists:keysearch("Executable", 1, Config),
    [XRes, YRes] = string:tokens(Resolution, "x"),
    os:cmd(Executable ++ " \"" ++ BrowserExec ++ "\" " ++ XRes ++ " " ++ YRes ++ " " ++ Url ++ " \"" ++ (Filename = Os ++ " " ++ Browser ++ " " ++ Resolution ++ ".jpg") ++ "\""),
    mail(Email, Filename, Config),
    file:delete(Filename),
    ok.

%% Send email with screenshot attached
mail(Email, Filename, Config) ->
    mail(Email, Filename, Config, 1).

mail(_Email, _Filename, _Config, Tries) when Tries > ?MAXMAILTRIES ->
    error;
mail(Email, Filename, Config, Tries) ->
    {value, {"FromAddress", FromAddress}} = lists:keysearch("FromAddress", 1, Config),
    {value, {"MessageSubject", MessageSubject}} = lists:keysearch("MessageSubject", 1, Config),
    {value, {"MessageBody", MessageBody}} = lists:keysearch("MessageBody", 1, Config),
    {ok, File} = file:read_file(Filename),
    Msg = email_msg:multi_msg(FromAddress, Email, MessageSubject, MessageBody, [{"image/jpeg", Filename, File}]),
    case catch smtp_fsm:sendemail(smtp, FromAddress, Email, Msg) of
	ok ->
	    ok;
	_Other -> % Something went wrong, probably we've lost the connection. Restart and try again.
	    catch smtp_fsm:close(smtp),
	    {value, {"SmtpHost", SmtpHost}} = lists:keysearch("SmtpHost", 1, Config),
	    smtpcon(SmtpHost),
	    catch smtp_fsm:sendemail(smtp, FromAddress, Email, Msg, Tries + 1)
    end.

%% Start and register smtp connection
smtpcon(Server) ->
    smtpcon(Server, 1).

smtpcon(_Server, Tries) when Tries > ?MAXMAILTRIES ->
    exit("Couldn't connect to SMTP");
smtpcon(Server, Tries) ->
    case smtp_fsm:start(Server) of
	{ok, Pid} ->
	    smtp_fsm:helo(Pid),
	    register(smtp, Pid);
	_Other ->
	    smtpcon(Server, Tries + 1)
    end.

%%%%%%%%%%%%% CONFIG HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config() ->
    {ok, Config} = file:read_file(?CONFIG_FILE),
    {ok, {json_object, ParsedConfig}} = json:decode_string(binary_to_list(Config)),
    ParsedConfig.

%% Generate a list of all available browsers
browsers(Config) ->
   {value, {"Browsers", BrowserList}} = lists:keysearch("Browsers", 1, Config),   
    StrippedBrowserList = lists:map( fun({json_object, DataList}) ->
					     {value, {"Desc", Desc}} = lists:keysearch("Desc", 1, DataList),
					     Desc
				     end,
				     tuple_to_list(BrowserList)),
    {value, {"Resolutions", ResolutionList}} = lists:keysearch("Resolutions", 1, Config),
    {value, {"OS", OS}} = lists:keysearch("OS", 1, Config),
    mashup(OS, StrippedBrowserList, tuple_to_list(ResolutionList)).

%% Create list of all browser/resolution-combinations possible
mashup2(Os, Browser, [Resolution | ResolutionTail]) ->
    [ {Os, Browser, Resolution} | mashup2(Os, Browser, ResolutionTail) ];

mashup2(_Os, _Browser, []) ->
    [].

mashup(Os, [Browser | BrowserTail], Resolutions) ->
    mashup2(Os, Browser, Resolutions) ++ mashup(Os, BrowserTail, Resolutions);

mashup(_Os, [], _Resolutions) ->
    [].

%% Get exec from browser desc
getexec(Browser, Config) ->
    {value, {"Browsers", BrowserList}} = lists:keysearch("Browsers", 1, Config),
    {ok, {json_object, ResultPropList}} = kdk:listsearch( fun({json_object, PropList}) ->
								  case lists:keysearch("Desc", 1, PropList) of
								      {value, {"Desc", Browser}} ->
									  true;
								      _Else ->
									  false
								  end
							  end,
							  tuple_to_list(BrowserList)),
    {value, {"Exec", Exec}} = lists:keysearch("Exec", 1, ResultPropList),
    Exec.





