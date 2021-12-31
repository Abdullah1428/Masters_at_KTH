% --------------------------------------------------------
% ID2201 - Distributed Systems Basic Course
% HW2: Routy - link state OSPF protocol
% Submitted by: Abdullah Abdullah
% --------------------------------------------------------

% Section 3 -> The Interfaces
% A router will also need to keep track of a set of interfaces. 
% A interface is described by the symbolic name (london), a 
% process reference and a process identifier.

-module(interface).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).


% --------------------------------------------------------
% new() -> return an empty set of interfaces.
new() ->
  [].

% --------------------------------------------------------
% add/4
% add(Name, Ref, Pid, Intf) add a new entry to the set and 
% return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
  [{Name,Ref,Pid} | Intf].

% --------------------------------------------------------
% remove/2
% remove(Name, Intf) remove an entry given a name of an 
% interface, return a new set of interfaces.
remove(Name,Intf) ->
  lists:keydelete(Name,1,Intf).

% --------------------------------------------------------
% lookup/2
% lookup(Name, Intf) find the process identifier given a 
% name, return {ok, Pid} if found otherwise notfound.
lookup(Name,Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {_Name,_Ref,Pid} ->
      {ok,Pid};
    false ->
      notfound
  end.

% --------------------------------------------------------
% ref/2
% ref(Name, Intf) find the reference given a name and 
% return {ok, Ref} or notfound.
ref(Name,Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {_Name,Ref,_Pid} ->
      {ok,Ref};
    false ->
      notfound
  end.

% --------------------------------------------------------
% name/2
% name(Ref, Intf) find the name of an entry given a 
% reference and return {ok, Name} or notfound.
name(Ref,Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name,Ref,_Pid} ->
      {ok, Name};
    false ->
      notfound
  end.

% --------------------------------------------------------
% list/1
% list(Intf) return a list with all names.
list(Intf) ->
  lists:map(
    fun({Name, _Ref, _Pid}) -> 
      Name 
    end, 
    Intf
  ).

% --------------------------------------------------------
% broadcast/2
% broadcast(Message, Intf) send the message to all interface 
% processes.
broadcast(_Message,[]) -> ok;
broadcast(Message,[{_Name,_Ref,Pid}|Remaining]) ->
  Pid ! Message,
  broadcast(Message,Remaining).
% --------------------------------------------------------