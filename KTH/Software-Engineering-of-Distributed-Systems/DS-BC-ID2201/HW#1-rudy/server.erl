% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW1: Rudy - a small web server
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% --------------------------------------------------------
% The server file
-module(server).
-export([start/1,start_pool/2,stop/0,stop_pool/0]).

% --------------------------------------------------------
% 2.2 -> a server
% start(Port) -> starts the server on give PORT
start(Port) ->
  register(rudy, spawn(fun() -> rudy:init(Port) end)).

% start(Port,N) -> starts the server on give PORT and N is for number of processes
start_pool(Port,N) ->
  register(rudy, spawn(fun() -> rudy:init_mulitple(Port,N) end)).

% stop() -> kills the server
stop() ->
  exit(whereis(rudy), "time to die").
% stop() -> kills the server with signal stop for better handling. -> resource from rudy4.erl
stop_pool() ->
  rudy ! stop.