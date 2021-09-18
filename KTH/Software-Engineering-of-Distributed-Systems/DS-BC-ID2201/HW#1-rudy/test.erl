-module(test).
-export([bench/2,bench_parallel/4]).

% adding some method to test multiple process requests
% first method = start(Host,Port,N) = host = ip, port = port number, N = number of processes, R = requests
bench_parallel(Host,Port,N,R) -> 
  Start = erlang:system_time(micro_seconds),
  run_parallel_tests(Host,Port,N,R,self()),
  collect(N),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.

bench(Host, Port) ->
  Start = erlang:system_time(micro_seconds),
  run(25, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  T = Finish - Start,
  io:format("~w ms~n", [T]).

% run_parallel to test multiple requests
run_parallel_tests(_, _, 0, _, _) ->
  ok;
run_parallel_tests(Host, Port, N, R, Ctrl) ->
  spawn(fun() -> test_request(Host, Port, R, Ctrl) end),
  run_parallel_tests(Host, Port, N-1, R, Ctrl).

test_request(Host, Port, R, Ctrl) ->
  run(R, Host, Port),
  Ctrl ! ok.

collect(0) ->
  ok;
collect(N) ->    
  receive 
	ok ->
	  collect(N-1)
  end.

run(0, _, _) ->
    ok;
run(R, Host, Port) ->
  if
    R == 0 -> 
      %io:format("called ~n"),
      ok;
    true ->
      %io:format("called ~n"),
      request(Host, Port),
      run(R-1, Host, Port)
end.

request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} -> 
      ok;
    {error, Error} ->
      io:format("test: error: ~w~n", [Error])
  end,
  gen_tcp:close(Server).
