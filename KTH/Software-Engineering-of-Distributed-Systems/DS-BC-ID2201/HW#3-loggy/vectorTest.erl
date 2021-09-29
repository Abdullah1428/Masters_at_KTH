% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Loggy - a logical time logger
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(vectorTest).
-export([run/2]).


run(Sleep, Jitter) ->
  Log = vectorLogger:start([john, paul, ringo, george]),
  A = vectorWorker:start(john, Log, 13, Sleep, Jitter,[john, paul, ringo, george]),
  B = vectorWorker:start(paul, Log, 26, Sleep, Jitter,[john, paul, ringo, george]),
  C = vectorWorker:start(ringo, Log, 39, Sleep, Jitter,[john, paul, ringo, george]),
  D = vectorWorker:start(george, Log, 52, Sleep, Jitter,[john, paul, ringo, george]),
  vectorWorker:peers(A, [B, C, D]),
  vectorWorker:peers(B, [A, C, D]),
  vectorWorker:peers(C, [A, B, D]),
  vectorWorker:peers(D, [A, B, C]),
  timer:sleep(5000),
  vectorLogger:stop(Log),
  vectorWorker:stop(A),
  vectorWorker:stop(B),
  vectorWorker:stop(C),
  vectorWorker:stop(D).
