% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Loggy - a logical time logger
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(vectorTime).
-export([zero/1,inc/2,merge/2,leq/2,clock/1,update/3,safe/2,newQueue/0,updateHoldBackQueue/4,logSafeHoldBackQueue/2]).

zero(Nodes) ->
  lists:map(fun(Node) -> {Node, 0} end, Nodes).

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
    {_Name,Counter} ->
      lists:keyreplace(Name, 1, Time, {Name,Counter+1});
    false ->
      [{Name,1} | Time]
end.


merge([], Time) ->
  Time;
merge([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name,erlang:max(Ti,Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name,Ti} |merge(Rest, Time)]
  end.


leq([], _) ->
  true;

leq([{Name, Ti}|Rest],Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest,Time);
      true ->
        false
      end; 
    false ->
      false 
  end.

clock(_) ->
  [].

update(From, Time, Clock) ->
  {Name,Tj} = lists:keyfind(From, 1, Time),
  case lists:keyfind(From, 1, Clock) of
    {From, _} ->
      lists:keyreplace(From, 1, Clock, {Name,Tj});
    false ->
      [{Name,Tj}| Clock]
  end.

safe(Time, Clock) ->
  leq(Time,Clock).

newQueue() ->
  [].

updateHoldBackQueue(From,Time,Msg,HoldBackQueue) ->
  NewEntry = {From,Time,Msg},
  UpdatedQueue = [NewEntry | HoldBackQueue],
  lists:keysort(2,UpdatedQueue).
  % List = [{aksel,[{aksel,3},{abdullah,2}],hello},{abdullah,[{aksel,1},{abdullah,2}],hello}]
  % output = [{abdullah,[{aksel,1},{abdullah,2}],hello},{aksel,[{aksel,3},{abdullah,2}],hello}]

logSafeHoldBackQueue(_,[]) ->
  [];
logSafeHoldBackQueue(Clock,HoldBackQueue) ->
  [{From,Time,Msg} | RestQueue] = HoldBackQueue,
  case safe(Time,Clock) of
    true -> 
      lamportLogger:log(From,Time,Msg),
      logSafeHoldBackQueue(Clock,RestQueue);
   false ->
      HoldBackQueue
  end.


%abdullah send a message to vasi  -> [{a, 1},{b, 2},{c, 0},{d, 2}]
%(b) = vasi receiver = [{a, 0},{b, 1},{c, 1},{d, 2}] -> local vector 
%-> merge  -> [{a, 1},{b, 2},{c, 1},{d, 2}] 
%-> inc -> [{a, 1},{b, 3},{c, 1},{d, 2}]

% List2 = [{aksel,[{aksel,3},{abdullah,2}],hello},{abdullah,[{aksel,1},{abdullah,2}],hello}]