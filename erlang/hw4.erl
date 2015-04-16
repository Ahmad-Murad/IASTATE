%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Andrew Guibert         %%
%%  ComS 430 - Homework 4  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hw4).
-export([tailFib/1, listCompare/1, erlQuickSort/1, doRun/5, pi/3, calcPi/2, noDups/2]).

%% Question 1
%% N : the Nth fibbonaci number to compute
tailFib(0) ->
	0;
tailFib(1) ->
	1;
tailFib(N) ->
	tailFib(N-1) + tailFib(N-2).

	
%% Question 2
%% Adding elements to the head of the list is much more efficient than adding them to the tail.
%% Comparison results:
%% Array Size | Add to Head | Add to Tail
%%  1000      | 1           | 1
%%  10000     | 1           | 342998
%%  100000    | 47000       | 39670839
%% (times in microseconds)
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
	
	
%% Question 3
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
	
	
%% Question 4
%% This is the driver method that the user will type in.
%% The number of samples and number of worker threads must be specified.
calcPi(NumSamples, TotalThreads) ->
	doRun(NumSamples, TotalThreads, TotalThreads, 0, 0).
	
%% All worker threads have been started and completed.  Compute Pi.
doRun(NumSamples, TotalThreads, 0, 0, Total) ->
	io:format("PI : ~w\n", [4.0 * Total / NumSamples]);
%% All worker threads have been started.  Listen for messages.
doRun(NumSamples, TotalThreads, 0, RunningThreads, Total) ->
	receive
		N ->
			doRun(NumSamples, TotalThreads, 0, RunningThreads - 1, Total + N)
	end;
%% Start worker threads.
doRun(NumSamples, TotalThreads, ToStart, RunningThreads, Total) ->
	spawn(?MODULE, pi, [(NumSamples div TotalThreads), 0, self()]),
	doRun(NumSamples, TotalThreads, ToStart-1, RunningThreads+1, 0).

%% All samples complete.  Send result message to parent process for computation.
pi(0, Count, ParentPid) ->
	ParentPid ! Count;
%% Compute 1 sample of pi.
pi(NumSamples, Count, ParentPid) ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	X = random:uniform(),
	Y = random:uniform(),
	if
		(X * X + Y * Y) < 1 ->
			pi(NumSamples - 1, Count + 1, ParentPid);
		true ->
			pi(NumSamples - 1, Count, ParentPid)
	end.
	
	
%% Question 5
noDups(Element, Acc) ->
	if
		Acc == [] ->
			lists:append([Element], Acc);
		hd(Acc) /= Element ->
			lists:append([Element], Acc);
		true ->
			Acc
	end.


	