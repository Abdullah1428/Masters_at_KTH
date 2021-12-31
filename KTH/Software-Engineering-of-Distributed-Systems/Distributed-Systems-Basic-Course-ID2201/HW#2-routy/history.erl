% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW2: Routy - link state OSPF protocol
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Section 4 -> The History
% When we send link-state messages around we need to avoid 
% cyclic paths; if we are not careful we will resend messages 
% forever. We can solve this in two ways, either we set a 
% counter on each message and decrement the counter in each hop, 
% hoping that it will reach all routers before the counter 
% reaches zero, or we keep track of what messages we have seen so far.

-module(history).
-export([new/1,update/3]).

% --------------------------------------------------------
% new/1
% new(Name) Return a new history, where messages from Name 
% will always be seen as old.
new(Name) ->
  [{Name, inf}].

% --------------------------------------------------------
% update/3
% update(Node, N, History) Check if message number N from the
% Node is old or new. If it is old then return old but if it is
% new return {new, Updated} where Updated is the updated history.
update(Node,N,History) ->
  case lists:keyfind(Node, 1, History) of
    false ->
      {new , [{Node, N}|History]};
    {_Name,Number} ->
      case N > Number of
        true ->
          {new, lists:keyreplace(Node,1,History,{Node,N})};
        false ->
          old
      end
  end.
% --------------------------------------------------------

