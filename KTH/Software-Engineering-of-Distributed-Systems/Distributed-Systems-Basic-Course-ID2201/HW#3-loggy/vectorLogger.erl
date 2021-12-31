% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Loggy - a logical time logger
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(vectorLogger).
-export([start/1, stop/1, log/3]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  % defining a clock which will keep track of nodes
  Clock = vectorTime:clock(Nodes),
  % defining initial hold back queue to be empty list
  HoldBackQueue = vectorTime:newQueue(),
  % loop method
  loop(Clock,HoldBackQueue).

loop(Clock,HoldBackQueue) ->
  receive
    {log, From, Time, Msg} ->
      % log msg received from a node at given time so we update clock.
      UpdatedClock = vectorTime:update(From,Time,Clock),
      % add this new message to hold-back queue.
      UpdatedHoldBackQueue = vectorTime:updateHoldBackQueue(From,Time,Msg,HoldBackQueue),
      % now we log the messages that are safe to log from the queue and return unsafe queue so that we can pass it back
      UnSafeHoldBackQueue = vectorTime:logSafeHoldBackQueue(UpdatedClock,UpdatedHoldBackQueue),
      % passing the updated clock and un safe queue to loop.
      loop(UpdatedClock,UnSafeHoldBackQueue);
    stop ->
      ok 
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

