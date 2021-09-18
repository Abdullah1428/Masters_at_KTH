% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW1: Rudy - a small web server
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% --------------------------------------------------------
% 2 The first reply
-module(rudy).
-export([init/1,init_mulitple/2]).

% --------------------------------------------------------
% 2.1 -> Socket API
% init(PORT) -> initialize the server with PORT number and
% open a listening socket and passes socket to handler
% upon handling request the socket will be closed
init(Port) ->
  % Opt
  % 1) list = to get bytes as list of integers instead of binary,
  % 2) {acive,false} = to read the inputs from recv/2 (recv(Socket,Length) instead of socket sending it to us as message , 
  % *) For understanding purpose -> The {active, true} flag means that the system actively reads packets from the socket 
  % and sends them to the controlling process message inbox as {tcp,Socket,Data}.
  % 3) {reuseaddr,true} = to use the port address again and again
  Opt = [list, {active, false}, {reuseaddr, true}], 
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      receive
        stop ->
          gen_tcp:close(Listen)
      end;
    {error, Error} ->
      io:format("Error from rudy: ~w~n", [Error]),
      error
  end.

% init(PORT,N) -> initialize the server with PORT and N number
% of processes for multiprocessing and open a listening socket
% and passes socket to handler upon handling request the socket will be closed
init_mulitple(Port,N) ->
  % Opt
  % 1) list = to get bytes as list of integers instead of binary,
  % 2) {acive,false} = to read the inputs from recv/2 (recv(Socket,Length) instead of socket sending it to us as message , 
  % *) For understanding purpose -> The {active, true} flag means that the system actively reads packets from the socket 
  % and sends them to the controlling process message inbox as {tcp,Socket,Data}.
  % 3) {reuseaddr,true} = to use the port address again and again
  Opt = [list, {active, false}, {reuseaddr, true},{backlog, 500}], 
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      %handler(Listen),
      multiple_handlers_pool(N,Listen),
      receive
        stop ->
          gen_tcp:close(Listen)
      end;
    {error, Error} ->
      io:format("Error from rudy [Listen]: ~w~n", [Error])
  end.

% method multiple_handlers_pool is for creating mulitple processes and
% then passing the handler to each process so we can do multi processing.
multiple_handlers_pool(0,_) ->
  ok;
multiple_handlers_pool(N,Listen) ->
  % spawn_link(Fun) -> pid() when Fun :: function().
  % Returns the process identifier of a new process started by the application of Fun to the empty list []. 
  % A link is created between the calling process and the new process, atomically.
  spawn_link(fun()-> handler(Listen) end),
  multiple_handlers_pool(N-1,Listen).

 
% --------------------------------------------------------
% handler(Listen) -> will listen to socket for incoming connection
% once connected it will pass the connection to request
% when the request is handled connection will close
handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client),
      % our connection closes after handling one request. To continue
      % accepting requests and handling it we have to call handler
      % recursively once the first request has been handled
      handler(Listen);
    {error, _} ->
      error
  end.

% --------------------------------------------------------
% request(Client) -> will read request from client connection
% and parse it using the http:parse_request method we defined
% and will pass the request to reply. The reply is sent back
% to the client 
request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} -> 
      % to see the str we receive
      %io:format("Str : ~s~n", [Str]),
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("Error from rudy [Request]: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

% --------------------------------------------------------
% reply(Request) -> here we can decide what to reply and 
% we will use our http:ok method we defined for the response
reply({{get, URI, _}, _, _}) ->
  %timer:sleep(100), % delay to mimick heavy request delay
  http:ok("Welcome to Rudy - a simple web server, The URI is: " ++ URI).

