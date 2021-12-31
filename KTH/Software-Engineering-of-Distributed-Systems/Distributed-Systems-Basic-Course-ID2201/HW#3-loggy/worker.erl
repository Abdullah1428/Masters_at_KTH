% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Loggy - a logical time logger
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> 
    init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  rand:seed(exsss,{Seed, Seed, Seed}),
  receive
    {peers, Peers} ->
      % place where we will initialize the counter
      loop(Name, Log, Peers, Sleep, Jitter,lamportTime:zero()); 
    stop -> 
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, C)->
  %io:format("Peers ~w~n",[Peers]),
  Wait = rand:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      % first step to merge the two lamport time stamps and increment it by one.
      MergedTime = lamportTime:merge(Time,C),
      IncrementTime = lamportTime:inc(Name,MergedTime),
      Log ! {log, Name, Time, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter,IncrementTime);
    stop -> 
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
    after Wait ->
      Selected = select(Peers),
      % when sening message increment counter
      Time = lamportTime:inc(Name,C),
      Message = {hello, rand:uniform(100)},
      Selected ! {msg, Time, Message},
      jitter(Jitter),
      Log ! {log, Name, Time, {sending, Message}},
      loop(Name, Log, Peers, Sleep, Jitter,Time)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> 
  ok;
jitter(Jitter) -> 
  timer:sleep(rand:uniform(Jitter)).