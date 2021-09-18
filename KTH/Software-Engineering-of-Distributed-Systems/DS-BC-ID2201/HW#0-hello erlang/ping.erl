-module(ping).
-export([hello/0]).

hello() ->
  receive
    X -> io:format("aaa! surprise, a message: ~~n",[X])
  end.
