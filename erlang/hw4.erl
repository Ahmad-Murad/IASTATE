-module(hw4).
-export([tailFib/1, listCompare/0]).

%% question 1
tailFib(0) ->
	0;
tailFib(1) ->
	1;
tailFib(N) ->
	tailFib(N-1) + tailFib(N-2).

	
%% question 2
listCompare() ->
	Start = erlang:now().
	End = erlang:now().
	Elapsed = timer:now_diff(End, Start).
	
listGenHead(N) ->
	