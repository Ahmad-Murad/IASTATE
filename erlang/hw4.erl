%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Andrew Guibert         %%
%%  ComS 430 - Homework 4  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hw4).
-export([tailFib/1, listCompare/1, erlQuickSort/1, doRun/4, pi/3]).

%% question 1
%% N : the Nth fibbonaci number to compute
tailFib(0) ->
	0;
tailFib(1) ->
	1;
tailFib(N) ->
	tailFib(N-1) + tailFib(N-2).

	
%% question 2
%% N : the size of the array to be randomly generated and sorted
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
%% N : the size of the array to be randomly generated and sorted
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
	
	
%% question 4
%% cd("R:/User Folders/Documents/GitHub/IASTATE/erlang").
doRun(NumSamples, 0, 0, Total) ->
	io:fwrite("PI : ~w\n", [4.0 * Total / NumSamples]);
doRun(NumSamples, 0, RemaingingThreads, Total) ->
	receive
		N ->
			%%io:fwrite("GOT MESSAGE ~w\n", [N])
			doRun(NumSamples, 0, RemaingingThreads - 1, Total + N)
	end;
doRun(NumSamples, NumThreads, _, _) ->
	spawn(hw4, pi, [NumSamples, 0, self()]),
	doRun(NumSamples, NumThreads - 1, NumThreads, 0).

pi(0, Count, RUN_PID) ->
	io:fwrite("Count is : ~w\n", [Count]),
	RUN_PID ! Count;
pi(NumSamples, Count, RUN_PID) ->
	X = random:uniform(),
	Y = random:uniform(),
	if
		(X * X) + (Y * Y) < 1 ->
			pi(NumSamples - 1, Count + 1, RUN_PID);
		true ->
			pi(NumSamples - 1, Count, RUN_PID)
	end.
	