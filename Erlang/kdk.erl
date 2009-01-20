-module(kdk).
-export([listsearch/2, randStr/1]).

%% Custom listsearch
listsearch(Fun, [Head | Tail]) ->
		case Fun(Head) of
			true ->
				{ok, Head};
			false ->
				listsearch(Fun, Tail)
		end;
		
listsearch(_Fun, []) ->
	false.

%% Random ascii-string of specified length
randStr(0) ->
    [];
randStr(Len) ->
    [ case random:uniform(52) of
	  Char when Char =< 26 ->
	      Char + 64;
	  Char ->
	      Char + 70
      end | randStr(Len - 1) ].    

