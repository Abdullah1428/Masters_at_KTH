-module(wait).
-export([hello/0]).

hello() ->
  receive
    X -> io:format("aaa! surprise, a message: ~s~n",[X])
  end.

% -----
% to run this program we can run following
% cmds in erl shell
%
% P = spawn(wait,hello,[]). 
% P ! "message-you-want-to-send-as-arg".
%
% Also we can register the process with identifier
% register("your-desire-name",P).
% "your-desire-name" ! "message-you-want-to-send-as-arg".
% -----