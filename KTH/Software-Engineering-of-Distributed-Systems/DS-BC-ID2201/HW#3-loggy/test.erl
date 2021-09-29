% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW3: Loggy - a logical time logger
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

-module(test).
-export([run/2]).


run(Sleep, Jitter) ->
  Log = lamportLogger:start([john, paul, ringo, george]),
  A = worker:start(john, Log, 13, Sleep, Jitter),
  B = worker:start(paul, Log, 26, Sleep, Jitter),
  C = worker:start(ringo, Log, 39, Sleep, Jitter),
  D = worker:start(george, Log, 52, Sleep, Jitter),
  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),
  timer:sleep(5000),
  lamportLogger:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D).
