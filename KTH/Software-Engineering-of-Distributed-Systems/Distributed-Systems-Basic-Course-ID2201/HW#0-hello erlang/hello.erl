-module(hello).

-export([world/0,pipe/1,vr/1]).

% hello:pipe(" GET"). -> should return [71,69,84]
world()-> 
  "Hello world!".

% hello:pipe(" GET"). -> should return [71,69,84]
% this here means that the world function 
% will expect first a space (32) ASCII values and then some word or string 
pipe([32 |N])-> 
  io:format("~w~n", [N]).

% to visualize the recursive function to understand rudy
vr(R0) ->
  {Headers,R1} = headers(R0),
  io:format("Headers: ~s~n : R1: ~s~n", [Headers,R1]).

headers([13,10|R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    io:format("Header1: ~s~n", [Rest]),
    {[Header|Rest], R2}.
    

header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    io:format("Header2: ~s~n", [Rest]),
    {[C|Rest], R1}.

% hello:vr("foo\r\n34\r\n\r\n").
    


