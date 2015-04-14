%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Andrew Guibert         %%
%%  ComS 430 - Homework 4  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hw4).
-export([tailFib/1, listCompare/1, erlQuickSort/1]).

%% question 1
tailFib(0) ->
	0;
tailFib(1) ->
	1;
tailFib(N) ->
	tailFib(N-1) + tailFib(N-2).

	
%% question 2
listCompare(N) ->
	Start = erlang:now(),
	HeadList = addToHead([], N),
	End = erlang:now(),
	Elapsed = timer:now_diff(End, Start),
	io:fwrite("The head list is: ~W\n", [HeadList, 20]),
	io:fwrite("Time for head list: ~w microseconds\n", [Elapsed]),
	Start2 = erlang:now(),
	TailList = addToTail([], N),
	End2 = erlang:now(),
	Elapsed2 = timer:now_diff(End2, Start2),
	io:fwrite("The tail list is: ~W\n", [TailList, 20]),
	io:fwrite("Time for tail list: ~w microseconds\n", [Elapsed2]).

addToHead(List, 0) ->
	List;
addToHead(List, N) ->
	addToHead([random:uniform(100)] ++ List, N-1).
	
addToTail(List, 0) ->
	List;
addToTail(List, N) ->
	addToTail(List ++ [random:uniform(100)], N-1).
	
	
%% question 3
%% Quick sort results:
%% Array Size | Erlang | Java
%%  100       | 1      | 206 
%%  1000      | 1      | 434
%%  10000     | 16000  | 3623
%%  100000    | 124000 | 10151
%% (times in microseconds)
erlQuickSort(N) ->
	ToSort = [random:uniform(N) || _ <- lists:seq(1,N)],
	Start = erlang:now(),
	Sorted = sort(ToSort),
	End = erlang:now(),
	Elapsed = timer:now_diff(End, Start),
	io:fwrite("The original list is: ~W\n", [ToSort, 20]),
	io:fwrite("The sorted   list is: ~W\n", [Sorted, 20]),
	io:fwrite("Sorting the list took: ~w microseconds\n", [Elapsed]).

sort([Pivot|T]) ->
    sort([ X || X <- T, X < Pivot]) ++
    [Pivot] ++
    sort([ X || X <- T, X >= Pivot]);
sort([]) -> [].
	