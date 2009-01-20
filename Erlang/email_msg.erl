%%
%% file: email_msg.erl
%% author: Michael Bradford <michael.bradford@t-mobile.uk.net>
%% description: a very simple module that creates a non-multipart
%% email message.
%% Doesn't support MIME or non-ascii characters
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usage Example
%%
%% 2> c(email_msg).
%% {ok,email_msg}
%% 3> From = "michael.bradford@t-mobile.uk.net".
%% "michael.bradford@t-mobile.uk.net"
%% 4> To = "test@test.co.uk".
%% "test@test.co.uk"
%% 5> Subject = "Testing !!!!".
%% "Testing !!!!"
%% 6> Content = "Hi Mike, this is a test, bye".
%% "Hi Mike, this is a test, bye"
%% 7> Msg = email_msg:simp_msg(From,To,Subject,Content).
%% "from: michael.bradford@t-mobile.uk.net\r\nto: test@test.co.uk\r\n
%% subject: Testi ng !!!!\r\n\r\nHi Mike, this is a test, bye\r\n"
%% 8> 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
-module(email_msg).
-vsn('v2.0').

-export([simp_msg/4, multi_msg/5]).

simp_msg(From, To, Subject, Message) ->
    FromStr = ["from: ", From, "\r\n"],
    ToStr = ["to: ", To, "\r\n"],
    SubjStr = ["subject: ", Subject, "\r\n"],
    MsgStr = ["\r\n", Message],
    lists:concat(lists:concat([FromStr, ToStr, SubjStr, MsgStr, ["\r\n"]])).

multi_msg(From, To, Subject, Message, Files) ->
    BoundStr = "__" ++ kdk:randStr(25) ++ "__",
    Msg = [ "From: ", From, "\r\n",
	    "To: ", To, "\r\n",
	    "Subject: ", Subject, "\r\n",
	    "MIME-Version: 1.0\r\n",
	    "Content-Type: multipart/mixed; boundary=\"", BoundStr, "\"\r\n",
	    "\r\nThis is a multi-part message in MIME format. Please use an appropiate client.\r\n", 
	    "--", BoundStr, "\r\n",
	    "Content-type: text/plain\r\n",
	    "\r\n", Message, "\r\n",
	    file_parts(BoundStr, Files),
	    "--", BoundStr, "--" ],
    lists:concat(Msg).

file_parts(BoundStr, [{MimeType, Filename, FileContents} | T]) ->
    [ "--", BoundStr, "\r\n",
      "Content-type: ", MimeType, "; name=\"", Filename, "\"\r\n",
      "Content-transfer-encoding: base64\r\n",
      "Content-disposition: attachment; filename=\"", Filename, "\"\r\n",
      "\r\n", base64:encode(FileContents), "\r\n" | file_parts(BoundStr, T) ];
file_parts(_BoundStr, []) ->
    [].
